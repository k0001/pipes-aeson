{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities similar to those exported by the
-- "Control.Proxy.Aeson" module, except they do not restrict the 'Ae.Value's
-- that might be encoded or decoded to be just valid top-level values. That is,
-- not only 'Ae.Object's or 'Ae.Array's, according to to the RFC-4627 JSON
-- standard.

module Pipes.Aeson.Unsafe
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

import           Control.Monad                    (unless)
import           Pipes
import qualified Pipes.Aeson.Internal             as I
import qualified Pipes.Attoparsec                 as PA
import qualified Pipes.Parse                      as Pp
import           Control.Monad.Trans.Either       (EitherT, left)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Aeson                       as Ae
import qualified Data.Aeson.Parser                as Ae (value')
import qualified Data.ByteString.Char8            as B

--------------------------------------------------------------------------------

-- | Like 'Control.Proxy.Aeson.encode', except it accepts any 'Ae.ToJSON'
-- instance.
encode :: (Monad m, Ae.ToJSON a) => a -> Producer B.ByteString m ()
encode = I.fromLazy . Ae.encode
{-# INLINABLE encode #-}

-- | Like 'Control.Proxy.Aeson.encodeD', except it accepts any 'Ae.ToJSON'
-- instance.
encodeD :: (Monad m, Ae.ToJSON a) => () -> Pipe a B.ByteString m r
encodeD = pull />/ encode
{-# INLINABLE encodeD #-}

--------------------------------------------------------------------------------

-- | Like 'Control.Proxy.Aeson.decode', except it will decode any 'Ae.ToJSON'
-- instance.
decode
  :: (Monad m, Ae.FromJSON r)
  => Client Pp.Draw (Maybe B.ByteString)
     (EitherT I.DecodingError (StateT [B.ByteString] m)) r
decode = do
    v <- hoist (I.bimapEitherT' I.ParserError id) $ PA.parse Ae.value'
    case Ae.fromJSON v of
      Ae.Error e   -> lift . left $ I.ValueError e
      Ae.Success r -> return r
{-# INLINABLE decode #-}

-- | Like 'Control.Proxy.Aeson.decodeD', except it will decode any 'Ae.ToJSON'
-- instance.
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

-- | Like 'Control.Proxy.Aeson.parseValue', except it will parse into any
-- 'Ae.Value'.
parseValue
  :: Monad m
  => Client Pp.Draw (Maybe B.ByteString)
     (EitherT PA.ParsingError (StateT [B.ByteString] m)) Ae.Value
parseValue = PA.parse Ae.value'
{-# INLINABLE parseValue #-}

-- | Like 'Control.Proxy.Aeson.parseValueD', except it will parse into any
-- 'Ae.Value'.
parseValueD
  :: Monad m
  => ()
  -> Proxy Pp.Draw (Maybe B.ByteString) () Ae.Value
     (EitherT PA.ParsingError (StateT [B.ByteString] m)) ()
parseValueD = \() -> loop where
    loop = do
        eof <- hoist lift $ I.skipSpace >> PA.isEndOfParserInput
        unless eof $ parseValue >>= respond >> loop
{-# INLINABLE parseValueD #-}

