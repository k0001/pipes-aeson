{-# LANGUAGE RankNTypes #-}

-- | This module allows you to encode and decode JSON values flowing downstream
-- through Pipes streams.
--
-- This module builds on top of the @pipes@ and @pipes-parse@ libraries, and
-- assumes you know how to use them.

module Pipes.Aeson
  ( -- * Top level JSON values
    -- $top-level-value
    TopLevelValue(..)
  , toTopLevelValue
    -- * Encoding
    -- $encoding
  , encode
    -- * Decoding
    -- $decoding
  , decode
    -- ** Consecutive elements
  , I.consecutively
    -- * Types
  , I.DecodingError(..)
  ) where


import           Pipes
import qualified Pipes.Aeson.Internal             as I
import qualified Pipes.Aeson.Unsafe               as U
import qualified Pipes.Attoparsec                 as PA
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Aeson                       as Ae
import qualified Data.ByteString.Char8            as B

--------------------------------------------------------------------------------
-- $top-level-value
--
-- The JSON RFC-4627 standard only allows 'Ae.Array' or 'Ae.Object' values as
-- top-level. The 'TopLevelValue' type used throughout this module in
-- replacement of Aesons's 'Ae.Value' enforces that restricion in a type-safe
-- manner.
--
-- If you want to ignore the standard and encode or decode any 'Ae.Value', then
-- use the facilities exported by the "Pipes.Aeson.Unsafe" module.
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
-- simpler 'encode'.
--
-- Both encoding proxies enforce the JSON RFC-4627 requirement that top-level
-- values are either 'Ae.Array's or 'Ae.Object's, as witnessed by the
-- 'TopLevelValue' type. However, if you need to ignore this requirement you may
-- use the similar encoding proxies exported by the "Pipes.Aeson.Unsafe"
-- module.

-- | Encodes the given 'TopLevelValue' as JSON and sends it downstream, possibly
-- in more than one 'B.ByteString' chunk.
encode :: Monad m => TopLevelValue -> Producer B.ByteString m ()
encode = U.encode
{-# INLINABLE encode #-}

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
-- 'decode', otherwise you may use the simpler 'decode'.
--
-- These proxies use the 'E.ErrorT' monad transformer to report decoding
-- errors, you might use any of the facilities exported by
-- "Control.Monad.Trans.Either" to recover from them.

-- | Decodes a top-level JSON value flowing downstream through the underlying
-- state.
--
-- * Compatible with the @pipes-parse@ facilities.
--
-- * In case of decoding errors, a 'I.DecodingError' exception is thrown in
-- the 'E.ErrorT' monad transformer.
--
-- * /Do not/ use this function if the underlying 'Producer' has leading empty
-- chunks or whitespace, otherwise you may get unexpected parsing errors.
--
-- This function decodes a single element. If instead, you want to consecutively
-- decode top-level JSON values flowing downstream, possibly skipping whitespace
-- in between them, use:
--
-- @
-- 'consecutively' 'decode'
--   :: ('Monad' m, 'Ae.FromJSON' b)
--   => 'Producer' 'B.ByteString' m r
--   -> 'Producer' ('Int', b) ('E.ErrorT' ('I.DecodingError', 'Producer' 'B.ByteString' m r) m) ()
-- @
decode
  :: (Monad m, Ae.FromJSON b)
  => S.StateT (Producer B.ByteString m r) m (Either I.DecodingError (Int, b))
decode = do
    ev <- PA.parse Ae.json'
    return $
      case ev of
        Left  e        -> Left (I.ParserError e)
        Right (len, v) -> do
          case Ae.fromJSON v of
            Ae.Error e   -> Left (I.ValueError e)
            Ae.Success b -> Right (len, b)
{-# INLINABLE decode #-}

