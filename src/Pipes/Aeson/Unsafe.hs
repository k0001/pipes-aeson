{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities similar to those exported by the
-- "Pipes.Aeson" module, except they do not restrict the 'Ae.Value's
-- that might be encoded or decoded to be just valid top-level values. That is,
-- not only 'Ae.Object's or 'Ae.Array's, according to to the RFC-4627 JSON
-- standard.

module Pipes.Aeson.Unsafe
  ( -- * Encoding
    -- $encoding
    encode
    -- * Decoding
    -- $decoding
  , decode
    -- ** Lower level parsing
  , parseValue
  ) where

import           Pipes
import qualified Pipes.Aeson.Internal             as I
import qualified Pipes.Attoparsec                 as PA
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Aeson                       as Ae
import qualified Data.Aeson.Parser                as Ae (value')
import qualified Data.ByteString                  as B

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.encode', except it accepts any 'Ae.ToJSON' instance.
encode :: (Monad m, Ae.ToJSON a) => a -> Producer B.ByteString m ()
encode = I.fromLazy . Ae.encode
{-# INLINE encode #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.decode', except it will decode any 'Ae.ToJSON' instance.
decode
  :: (Monad m, Ae.FromJSON b)
  => S.StateT (Producer B.ByteString m r) m (Either I.DecodingError (Int, b))
decode = do
    ev <- PA.parse Ae.value'
    return $
      case ev of
        Left  e        -> Left (I.ParserError e)
        Right (len, v) -> do
          case Ae.fromJSON v of
            Ae.Error e   -> Left (I.ValueError e)
            Ae.Success b -> Right (len, b)
{-# INLINABLE decode #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.parseValue', except it will parse into any 'Ae.Value'.
parseValue
  :: Monad m
  => S.StateT (Producer B.ByteString m r) m (Either PA.ParsingError (Int, Ae.Value))
parseValue = PA.parse Ae.value'
{-# INLINE parseValue #-}

