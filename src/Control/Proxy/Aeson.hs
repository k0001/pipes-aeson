{-# LANGUAGE DeriveDataTypeable #-}

-- | This module allows you to encode and decode JSON values flowing downstream
-- through Pipes streams, possibly interleaving other stream effects.
--
-- This module builds on top of the @pipes-parse@ package and assumes
-- you have read "Control.Proxy.Parse.Tutorial".

module Control.Proxy.Aeson
  ( -- * Encoding
    -- $encoding
    encode
  , encodeD
    -- * Decoding
    -- $decoding
  , decode
  , decodeD
  , DecodingError(..)
    -- ** Lower level parsing
  , parseJSON
  , parseJSOND
  , parseValue
  , parseValueD
  ) where

import           Control.Exception             (Exception)
import           Control.Monad                 (unless)
import qualified Control.Proxy                 as P
import qualified Control.Proxy.Attoparsec      as PA
import qualified Control.Proxy.Parse           as Pp
import qualified Control.Proxy.Trans.Either    as P
import qualified Control.Proxy.Trans.State     as P
import qualified Data.Aeson                    as Ae
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Char                     as Char
import           Data.Data                     (Data, Typeable)
import           Data.Function                 (fix)

--------------------------------------------------------------------------------

-- | An error while decoding a JSON value.
data DecodingError
  = ParserError PA.ParsingError
    -- ^An Attoparsec error that happened while parsing the raw JSON string.
  | ValueError String
    -- ^An Aeson error that happened while trying to convert a 'Ae.Value' to
    --  an 'A.FromJSON' instance, as reported by 'Ae.Error'.
  deriving (Show, Eq, Data, Typeable)

instance Exception DecodingError

--------------------------------------------------------------------------------
-- $decoding
--
-- /JSON decoding/ in Haskell involves two different steps:
--
-- * Parsing a raw JSON 'B.ByteString' as an Aeson 'Ae.Value'.
--
-- * Converting the obtained Aeson 'Ae.Value' to a 'Ae.FromJSON' instance.
--
-- Any of those steps can fail, and in case of errors, the 'DecodingError' type
-- explicitly states the step at which the error happened.
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
-- If instead you want to perform each of the decoding steps separately, perhaps
-- for efficiency reasons, you should use instead the 'parseJSON', 'parseJSOND',
-- 'parseValue' and 'parseValueD' proxies.


-- | Decodes one JSON value flowing downstream.
--
-- * In case of decoding errors, a 'DecodingError' exception is thrown in
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
-- > loop = do
-- >     -- Skip any leading whitespace and check that we haven't reached EOF.
-- >     eof <- liftP $ skipSpace >> isEndOfParserInput
-- >     unless eof $ do
-- >         -- 1. Possibly perform some stream effects here.
-- >         -- 2. Decode one JSON element from the stream.
-- >         exampleElement <- decode
-- >         -- 3. Do something with exampleElement and possibly perform
-- >         --    some more stream effects.
-- >         -- 4. Start all over again.
-- >         loop

-- We could implemente 'decode' in terms of 'parseJSON' and 'parseValue', but
-- this is arguably more efficient and readable.
decode
  :: (Monad m, P.Proxy p, Ae.FromJSON r)
  => P.EitherP DecodingError (P.StateP [B.ByteString] p)
     () (Maybe B.ByteString) y' y m r
decode = do
    ev <- P.liftP . P.runEitherP $ PA.parse Ae.json'
    case ev of
      Left e  -> P.throw (ParserError e)
      Right v ->
        case Ae.fromJSON v of
          Ae.Error e   -> P.throw (ValueError e)
          Ae.Success r -> return r
{-# INLINABLE decode #-}

-- | Decodes consecutive JSON values flowing downstream until end of input.
--
-- * In case of decoding errors, a 'DecodingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * Empty input chunks flowing downstream and whitespace in between JSON
-- values will be discarded.
decodeD
  :: (Monad m, P.Proxy p, Ae.FromJSON b)
  => ()
  -> P.Pipe (P.EitherP DecodingError (P.StateP [B.ByteString] p))
     (Maybe B.ByteString) b m ()
decodeD = \() -> loop
  where
    loop = do
        eof <- P.liftP $ skipSpace >> PA.isEndOfParserInput
        unless eof $ decode >>= P.respond >> loop
{-# INLINABLE decodeD #-}

--------------------------------------------------------------------------------

-- | Parses a raw JSON 'B.ByteString' as an Aeson 'Ae.Value'.
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
parseJSON
  :: (Monad m, P.Proxy p)
  => P.EitherP PA.ParsingError (P.StateP [B.ByteString] p)
     () (Maybe B.ByteString) y' y m Ae.Value
parseJSON = PA.parse Ae.json'
{-# INLINABLE parseJSON #-}


-- | Parses consecutive raw JSON 'B.ByteStrings' flowing downstream as
-- Aeson 'Ae.Value's, until end of input.
--
-- * In case of parsing errors, a 'DecodingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * Empty input chunks flowing downstream and whitespace in between JSON
-- values will be discarded.
parseJSOND
  :: (Monad m, P.Proxy p)
  => ()
  -> P.Pipe (P.EitherP PA.ParsingError (P.StateP [B.ByteString] p))
     (Maybe B.ByteString) Ae.Value m ()
parseJSOND = \() -> loop
  where
    loop = do
        eof <- P.liftP $ skipSpace >> PA.isEndOfParserInput
        unless eof $ parseJSON >>= P.respond >> loop
{-# INLINABLE parseJSOND #-}

--------------------------------------------------------------------------------

-- | Converts an Aeson 'Ae.Value' flowing downstream to a 'Ae.FromJSON'
-- instance.
--
-- In case of parsing errors, a 'String' exception holding the value provided
-- by Aeson's 'Ae.Error' is thrown in the 'Pe.EitherP' proxy transformer.
--
-- See the documentation of 'decode' for an example of how to interleave
-- other stream effects together with this proxy.
parseValue
  :: (Monad m, P.Proxy p, Ae.FromJSON r)
  => x -> P.EitherP String p x Ae.Value y' y m r
parseValue = \x -> do
    v <- P.request x
    case Ae.fromJSON v of
      Ae.Error e   -> P.throw e
      Ae.Success r -> return r
{-# INLINABLE parseValue #-}

-- | Converts Aeson 'Ae.Value's flowing downstream to a 'Ae.FromJSON' instance.
--
-- In case of parsing errors, a 'String' exception holding the value provided
-- by Aeson's 'Ae.Error' is thrown in the 'Pe.EitherP' proxy transformer.
parseValueD
  :: (Monad m, P.Proxy p, Ae.FromJSON b)
  => x -> P.EitherP String p x Ae.Value x b m r
parseValueD = parseValue P.\>\ P.pull
{-# INLINABLE parseValueD #-}


--------------------------------------------------------------------------------
-- $encoding
--
-- There are two different JSON encoding facilities exported by this module, and
-- choosing between them is easy: If you need to interleave JSON encoding
-- with other stream effects you must use 'encode', otherwise you may use the
-- simpler 'encodeD'.

-- | Encodes the given 'Ae.ToJSON' instance and sends it downstream, possibly in
-- more than one 'BS.ByteString' chunks.
encode
  :: (P.Proxy p, Monad m, Ae.ToJSON r)
  => r -> p x' x () B.ByteString m ()
encode = \r -> P.runIdentityP $ do
    BLI.foldrChunks (\e a -> P.respond e >> a) (return ()) (Ae.encode r)
{-# INLINABLE encode #-}


-- | Encodes 'Ae.ToJSON' instances flowing downstream, each in possibly more
-- than one 'BS.ByteString' chunks.
encodeD
  :: (P.Proxy p, Monad m, Ae.ToJSON a)
  => () -> P.Pipe p a B.ByteString m r
encodeD = P.pull P./>/ encode
{-# INLINABLE encodeD #-}

--------------------------------------------------------------------------------

-- | Consume and discard leading 'I.ParserInput' characters from upstream as
-- long as the given predicate holds 'True'.

-- XXX: we define 'skipSpace' here until 'pipes-bytestring' is released.
skipSpace
  :: (Monad m, P.Proxy p)
  => P.StateP [B.ByteString] p () (Maybe B.ByteString) y' y m ()
skipSpace = fix $ \loop -> do
    ma <- Pp.draw
    case ma of
      Nothing -> return ()
      Just a  -> do
        let a' = B.dropWhile Char.isSpace a
        if B.null a'
           then loop
           else Pp.unDraw a'
{-# INLINABLE skipSpace #-}
