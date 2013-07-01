{-# LANGUAGE RankNTypes #-}

-- | This module allows you to encode and decode JSON values flowing downstream
-- through Pipes streams, possibly interleaving other stream effects.
--
-- This module builds on top of the @pipes-parse@ and @pipes-attoparsec@
-- packages and assumes you have read "Pipes.Parse.Tutorial".

module Pipes.Aeson
  ( -- * Top level JSON values
    -- $top-level-value
    TopLevelValue(..)
  , toTopLevelValue
    -- * Encoding
    -- $encoding
  , encode
  , encodeD
    -- * Decoding
    -- $decoding
  , decode
  , decodeD
    -- ** Lower level parsing
  , parseValue
  , parseValueD
  , fromValue
  , fromValueD
    -- * Types
  , I.DecodingError(..)
  ) where


import           Control.Monad                    (unless)
import           Pipes
import qualified Pipes.Parse                      as Pp
import qualified Pipes.Aeson.Internal             as I
import qualified Pipes.Aeson.Unsafe               as U
import qualified Pipes.Attoparsec                 as PA
import           Control.Monad.Trans.Either       (EitherT, left)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Aeson                       as Ae
import qualified Data.ByteString.Char8            as B
import           Data.Maybe                       (fromJust)

--------------------------------------------------------------------------------
-- $top-level-value
--
-- The JSON RFC-4627 standard only allows 'Ae.Array' or 'Ae.Object' values as
-- top-level. The 'TopLevelValue' type used throughout this module in
-- replacement of Aesons's 'Ae.Value' enforces that restricion in a type-safe
-- manner.
--
-- If you want to ignore the standard and encode or decode any 'Ae.Value', then
-- use the facilities exported by the "Control.Proxy.Aeson.Unsafe" module.
--
-- * You may use the 'toTopLevelValue' function to convert any 'Ae.ToJSON'
-- instance to a 'TopLevelValue', if possible. Remember that 'Ae.Value' is
-- one such instance.
--
-- * Use the 'Ae.toJSON' method on a 'TopLevelValue' to obtain its underlying
-- 'Ae.Value' representation.


-- | A JSON top-level value must be an 'Ae.Object' or an 'Ae.Array', according
-- to RFC-4627.
data TopLevelValue
  = Object !Ae.Object
  | Array  !Ae.Array
  deriving (Show, Eq)

instance Ae.ToJSON TopLevelValue where
  toJSON (Object o) = Ae.Object o
  toJSON (Array  a) = Ae.Array  a

instance Ae.FromJSON TopLevelValue where
  parseJSON (Ae.Object o) = return (Object o)
  parseJSON (Ae.Array a)  = return (Array a)
  parseJSON _             = fail "Not a valid top-level value"

-- | Converts the given 'Ae.ToJSON' instance to a 'TopLevelValue' as long as its
-- 'Ae.Value' representation is one of 'Ae.Object' or 'Ae.Array', otherwise
-- 'Nothing'. Remember that 'Ae.Value' itself is a 'Ae.ToJSON' instance.
toTopLevelValue :: Ae.ToJSON a => a -> Maybe TopLevelValue
toTopLevelValue = \a ->
    case Ae.toJSON a of
      Ae.Object x -> Just (Object x)
      Ae.Array  x -> Just (Array  x)
      _           -> Nothing
{-# INLINABLE toTopLevelValue #-}

--------------------------------------------------------------------------------
-- $encoding
--
-- There are two different JSON encoding facilities exported by this module, and
-- choosing between them is easy: If you need to interleave JSON encoding
-- with other stream effects you must use 'encode', otherwise you may use the
-- simpler 'encodeD'.
--
-- Both encoding proxies enforce the JSON RFC-4627 requirement that top-level
-- values are either 'Ae.Array's or 'Ae.Object's, as witnessed by the
-- 'TopLevelValue' type. However, if you need to ignore this requirement you may
-- use the similar encoding proxies exported by the "Control.Proxy.Aeson.Unsafe"
-- module.

-- | Encodes the given 'TopLevelValue' as JSON and sends it downstream, possibly
-- in more than one 'BS.ByteString' chunk.
encode :: Monad m => TopLevelValue -> Producer B.ByteString m ()
encode = U.encode
{-# INLINABLE encode #-}

-- | Encodes 'TopLevelValue's flowing downstream as JSON, each in possibly more
-- than one 'BS.ByteString' chunk, and sends each chunk downstream.
encodeD :: Monad m => () -> Pipe TopLevelValue B.ByteString m r
encodeD = U.encodeD
{-# INLINABLE encodeD #-}

--------------------------------------------------------------------------------
-- $decoding
--
-- Decoding a JSON value as a Haskell type in involves two different steps:
--
-- * Parsing a raw JSON 'B.ByteString' into a 'TopLevelValue'.
--
-- * Converting the obtained 'TopLevelValue' to the desired type, which must be
-- a 'Ae.FromJSON' instance.
--
-- Any of those steps can fail, and in case of errors, the 'I.DecodingError'
-- type explicitly states at which the step the error happened.
--
-- There are two different JSON decoding facilities exported by this module,
-- both perform those steps at once. Choosing between them is easy: If you
-- need to interleave JSON decoding with other stream effects you must use
-- 'decode', otherwise you may use the simpler 'decodeD'.
--
-- These proxies use the 'P.EitherP' proxy transformer to report decoding
-- errors, you might use any of the facilities exported by
-- "Control.Proxy.Trans.Either" to recover from them.
--
-- If you prefer to perform each of the decoding steps separately, you
-- could use instead the 'parseValue', 'parseValueD', 'fromValue' or
-- 'fromValueD' proxies.


-- | Decodes one JSON value flowing downstream.
--
-- * In case of decoding errors, a 'I.DecodingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * /Do not/ use this proxy if your stream has leading empty chunks or
-- whitespace, otherwise you may get unexpected parsing errors.
--
-- Here is an example parsing loop that allows interleaving stream effects
-- together with 'decode':
--
-- @
--   loop = do
--       -- Skip any leading whitespace and check that we haven't reached EOF.
--       eof &#x3c;- 'P.liftP' $ 'Control.Proxy.ByteString.dropWhile' 'Data.Char.isSpace' >> 'PA.isEndOfParserInput'
--       'unless' eof $ do
--           -- 1. Possibly perform some stream effects here.
--           -- 2. Decode one JSON element from the stream.
--           exampleElement <- 'decode'
--           -- 3. Do something with exampleElement and possibly perform
--           --    some more stream effects.
--           -- 4. Start all over again.
--           loop
-- @
decode
  :: (Monad m, Ae.FromJSON r)
  => Client Pp.Draw (Maybe B.ByteString)
     (EitherT I.DecodingError (StateT [B.ByteString] m)) r
decode = do
    v <- hoist (I.bimapEitherT' I.ParserError id) $ PA.parse Ae.json'
    case Ae.fromJSON v of
      Ae.Error e   -> lift . left $ I.ValueError e
      Ae.Success r -> return r
{-# INLINABLE decode #-}


-- | Decodes consecutive JSON values flowing downstream until end of input.
--
-- * In case of decoding errors, a 'I.DecodingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * Empty input chunks flowing downstream and whitespace in between JSON
-- values will be discarded.
decodeD
  :: (Monad m, Ae.FromJSON b)
  => ()
  -> Proxy Pp.Draw (Maybe B.ByteString) () b
     (EitherT I.DecodingError (StateT [B.ByteString] m)) ()
decodeD = \() -> loop where
    loop = do
        eof <- hoist lift $ I.skipSpace >> PA.isEndOfParserInput
        unless eof $ decode >>= respond >> loop
{-# INLINABLE decodeD #-}

--------------------------------------------------------------------------------

-- | Parses a JSON value flowing downstream into a 'TopLevelValue'.
--
-- * In case of parsing errors, a 'PA.ParsingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * /Do not/ use this proxy if your stream has leading empty chunks or
-- whitespace, otherwise you may get unexpected parsing errors.
--
-- See the documentation of 'decode' for an example of how to interleave
-- other stream effects together with this proxy.
parseValue
  :: Monad m
  => Client Pp.Draw (Maybe B.ByteString)
     (EitherT PA.ParsingError (StateT [B.ByteString] m)) TopLevelValue
parseValue = return . fromJust . toTopLevelValue =<< PA.parse Ae.json'
{-# INLINABLE parseValue #-}


-- | Parses consecutive JSON values flowing downstream as 'TopLevelValue's,
-- until end of input.
--
-- * In case of parsing errors, a 'I.DecodingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * Empty input chunks flowing downstream and whitespace in between JSON
-- values will be discarded.
parseValueD
  :: Monad m
  => ()
  -> Proxy Pp.Draw (Maybe B.ByteString) () TopLevelValue
     (EitherT PA.ParsingError (StateT [B.ByteString] m)) ()
parseValueD = \() -> loop where
    loop = do
        eof <- hoist lift $ I.skipSpace >> PA.isEndOfParserInput
        unless eof $ parseValue >>= respond >> loop
{-# INLINABLE parseValueD #-}

--------------------------------------------------------------------------------

-- | Converts any 'Ae.Value' flowing downstream to a 'Ae.FromJSON' instance.
--
-- * In case of parsing errors, a 'String' exception holding the value provided
-- by Aeson's 'Ae.Error' is thrown in the 'Pe.EitherP' proxy transformer.
--
-- See the documentation of 'decode' for an example of how to interleave
-- other stream effects together with this proxy.
fromValue
  :: (Monad m, Ae.FromJSON r) => x -> Client x Ae.Value (EitherT String m) r
fromValue = \x -> do
    v <- request x
    case Ae.fromJSON v of
      Ae.Error e   -> lift (left e)
      Ae.Success r -> return r
{-# INLINABLE fromValue #-}


-- | Converts any 'Ae.Value's flowing downstream to 'Ae.FromJSON' instances and
-- forwards them downstream.
--
-- * In case of parsing errors, a 'String' exception holding the value provided
-- by Aeson's 'Ae.Error' is thrown in the 'Pe.EitherP' proxy transformer.
fromValueD
  :: (Monad m, Ae.FromJSON b) => x -> Proxy x Ae.Value x b (EitherT String m) r
fromValueD = fromValue \>\ pull
{-# INLINABLE fromValueD #-}
