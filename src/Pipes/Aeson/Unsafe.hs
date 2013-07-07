{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities similar to those exported by the
-- "Pipes.Aeson" module, except they do not restrict the 'Ae.Value's
-- that might be encoded or decoded to be just valid top-level values. That is,
-- not only 'Ae.Object's or 'Ae.Array's, according to to the RFC-4627 JSON
-- standard.

module Pipes.Aeson.Unsafe
  (-- * Encoding
    -- $encoding
    encodeOne
  , encode
    -- * Decoding
    -- $decoding
  , decodeOne
  , decode
    -- ** Lower level parsing
  , parseValueOne
  , parseValue
    -- * Types
  , I.DecodingError(..)
  ) where

import           Control.Monad                    (unless)
import           Pipes
import qualified Pipes.Aeson.Internal             as I
import qualified Pipes.Attoparsec                 as PA
import           Pipes.Lift                       (runErrorP)
import qualified Pipes.Parse                      as Pp
import qualified Control.Monad.Trans.Error        as E
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Aeson                       as Ae
import qualified Data.Aeson.Parser                as Ae (value')
import qualified Data.ByteString.Char8            as B

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.encodeOne', except it accepts any 'Ae.ToJSON' instance.
encodeOne :: (Monad m, Ae.ToJSON a) => a -> Producer B.ByteString m ()
encodeOne = I.fromLazy . Ae.encode
{-# INLINABLE encodeOne #-}

-- | Like 'Pipes.Aeson.encode', except it accepts any 'Ae.ToJSON' instance.
encode :: (Monad m, Ae.ToJSON a) => () -> Pipe a B.ByteString m r
encode = pull />/ encodeOne
{-# INLINABLE encode #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.decodeOne', except it will decode any 'Ae.ToJSON' instance.
decodeOne
  :: (Monad m, Ae.FromJSON r)
  => Client Pp.Draw (Maybe B.ByteString)
     (E.ErrorT I.DecodingError (StateT [B.ByteString] m)) r
decodeOne = do
    ev <- hoist lift (runErrorP (PA.parseOne Ae.value'))
    case ev of
      Left e -> lift (E.throwError (I.ParserError e))
      Right v -> do
        case Ae.fromJSON v of
          Ae.Error e   -> lift (E.throwError (I.ValueError e))
          Ae.Success r -> return r
{-# INLINABLE decodeOne #-}

-- | Like 'Pipes.Aeson.decode', except it will decode any 'Ae.ToJSON' instance.
decode
  :: (Monad m, Ae.FromJSON b)
  => ()
  -> Proxy Pp.Draw (Maybe B.ByteString) () b
     (E.ErrorT I.DecodingError (StateT [B.ByteString] m)) ()
decode = \() -> loop where
    loop = do
        eof <- hoist lift $ I.skipSpace >> PA.isEndOfParserInput
        unless eof $ decodeOne >>= respond >> loop
{-# INLINABLE decode #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.parseValueOne', except it will parse into any 'Ae.Value'.
parseValueOne
  :: Monad m
  => Client Pp.Draw (Maybe B.ByteString)
     (E.ErrorT PA.ParsingError (StateT [B.ByteString] m)) Ae.Value
parseValueOne = PA.parseOne Ae.value'
{-# INLINABLE parseValueOne #-}

-- | Like 'Pipes.Aeson.parseValue', except it will parse into any 'Ae.Value'.
parseValue
  :: Monad m
  => ()
  -> Proxy Pp.Draw (Maybe B.ByteString) () Ae.Value
     (E.ErrorT PA.ParsingError (StateT [B.ByteString] m)) ()
parseValue = \() -> loop where
    loop = do
        eof <- hoist lift $ I.skipSpace >> PA.isEndOfParserInput
        unless eof $ parseValueOne >>= respond >> loop
{-# INLINABLE parseValue #-}

