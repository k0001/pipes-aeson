{-# LANGUAGE DeriveDataTypeable #-}

module Control.Proxy.Aeson
  ( -- * Decoding
    decodeD
  , decode
    -- * Types
  , DecodingError(..)
  ) where

import           Control.Exception          (Exception)
import           Control.Monad              (unless)
import qualified Control.Proxy              as P
import qualified Control.Proxy.Attoparsec   as PA
import qualified Control.Proxy.Trans.Either as P
import qualified Control.Proxy.Trans.State  as P
import qualified Data.Aeson                 as Ae
import qualified Data.ByteString.Char8      as B
import           Data.Char                  (isSpace)
import           Data.Data                  (Data, Typeable)

--------------------------------------------------------------------------------

-- | An error that while decoding a JSON value.
data DecodingError
  = ParserError PA.ParsingError
    -- ^An Attoparsec error that happened while parsing the raw JSON string.
  | ValueError String
    -- ^An Aeson error that happened while trying to convert a 'Ae.Value' to
    --  an 'Ae.FromJSON' instance.
  deriving (Show, Eq, Data, Typeable)

instance Exception DecodingError

--------------------------------------------------------------------------------

decode
  :: (Monad m, P.Proxy p, Ae.FromJSON r)
  => P.EitherP DecodingError (P.StateP [B.ByteString] p)
     () (Maybe B.ByteString) y' y m r
decode = do
    ev <- P.liftP . P.runEitherP $ PA.parse Ae.json'
    case ev of
      Left e  -> P.throw (ParserError e)
      Right v -> do
          case Ae.fromJSON v of
            Ae.Error e   -> P.throw (ValueError e)
            Ae.Success r -> return r
{-# INLINABLE decode #-}


decodeD
  :: (Monad m, P.Proxy p, Ae.FromJSON b)
  => ()
  -> P.Pipe (P.EitherP DecodingError (P.StateP [B.ByteString] p))
     (Maybe B.ByteString) b m ()
decodeD = \() -> loop
  where
    loop = do
        eof <- P.liftP $ PA.isEndOfUsefulParserInput isSpace
        unless eof $ do
          () <- P.respond =<< decode
          loop
{-# INLINABLE decodeD #-}

