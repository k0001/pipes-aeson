-- | This module exports facilities similar to those exported by the
-- "Control.Proxy.Aeson" module, except they do not restrict the 'Ae.Value's
-- that might be encoded or decoded to be just valid top-level values. That is,
-- not only 'Ae.Object's or 'Ae.Array's, according to to the RFC-4627 JSON
-- standard.

module Control.Proxy.Aeson.Unsafe
  (-- * Encoding
    -- $encoding
    encode
  , encodeD
    -- * Decoding
    -- $decoding
  , decode
  , decodeD
    -- ** Lower level parsing
  , parseValue
  , parseValueD
    -- * Types
  , I.DecodingError(..)
  ) where

import           Control.Monad                 (unless)
import qualified Control.Proxy                 as P
import qualified Control.Proxy.Aeson.Internal  as I
import qualified Control.Proxy.Attoparsec      as PA
import qualified Control.Proxy.Trans.Either    as P
import qualified Control.Proxy.Trans.State     as P
import qualified Data.Aeson                    as Ae
import qualified Data.Aeson.Parser             as Ae (value')
import qualified Data.ByteString.Char8         as B

--------------------------------------------------------------------------------

-- | Like 'Control.Proxy.Aeson.encode', except it accepts any 'Ae.ToJSON'
-- instance.
encode
  :: (P.Proxy p, Monad m, Ae.ToJSON a) => a -> p x' x () B.ByteString m ()
encode = I.fromLazy . Ae.encode
{-# INLINABLE encode #-}

-- | Like 'Control.Proxy.Aeson.encodeD', except it accepts any 'Ae.ToJSON'
-- instance.
encodeD
  :: (P.Proxy p, Monad m, Ae.ToJSON a) => () -> P.Pipe p a B.ByteString m r
encodeD = P.pull P./>/ encode
{-# INLINABLE encodeD #-}

--------------------------------------------------------------------------------

-- | Like 'Control.Proxy.Aeson.decode', except it will decode any 'Ae.ToJSON'
-- instance.
decode
  :: (Monad m, P.Proxy p, Ae.FromJSON r)
  => P.EitherP I.DecodingError (P.StateP [B.ByteString] p)
     () (Maybe B.ByteString) y' y m r
decode = do
    ev <- P.liftP . P.runEitherP $ PA.parse Ae.value'
    case ev of
      Left e  -> P.throw (I.ParserError e)
      Right v ->
        case Ae.fromJSON v of
          Ae.Error e   -> P.throw (I.ValueError e)
          Ae.Success r -> return r
{-# INLINABLE decode #-}

-- | Like 'Control.Proxy.Aeson.decodeD', except it will decode any 'Ae.ToJSON'
-- instance.
decodeD
  :: (Monad m, P.Proxy p, Ae.FromJSON b)
  => ()
  -> P.Pipe (P.EitherP I.DecodingError (P.StateP [B.ByteString] p))
     (Maybe B.ByteString) b m ()
decodeD = \() -> loop where
    loop = do
        eof <- P.liftP $ I.skipSpace >> PA.isEndOfParserInput
        unless eof $ decode >>= P.respond >> loop
{-# INLINABLE decodeD #-}

--------------------------------------------------------------------------------

-- | Like 'Control.Proxy.Aeson.parseValue', except it will parse into any
-- 'Ae.Value'.
parseValue
  :: (Monad m, P.Proxy p)
  => P.EitherP PA.ParsingError (P.StateP [B.ByteString] p)
     () (Maybe B.ByteString) y' y m Ae.Value
parseValue = PA.parse Ae.value'
{-# INLINABLE parseValue #-}

-- | Like 'Control.Proxy.Aeson.parseValueD', except it will parse into any
-- 'Ae.Value'.
parseValueD
  :: (Monad m, P.Proxy p)
  => ()
  -> P.Pipe (P.EitherP PA.ParsingError (P.StateP [B.ByteString] p))
     (Maybe B.ByteString) Ae.Value m ()
parseValueD = \() -> loop where
    loop = do
        eof <- P.liftP $ I.skipSpace >> PA.isEndOfParserInput
        unless eof $ parseValue >>= P.respond >> loop
{-# INLINABLE parseValueD #-}

