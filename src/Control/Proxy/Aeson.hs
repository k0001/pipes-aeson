{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Proxy.Aeson
  ( decodeD
    -- * Types
  , JSONError(..)
  ) where

import           Control.Exception          (Exception)
import qualified Control.Proxy              as P
import qualified Control.Proxy.Attoparsec   as Pa
import qualified Control.Proxy.Trans.Either as P
import qualified Control.Proxy.Trans.State  as P
import qualified Data.Aeson                 as Ae
import qualified Data.ByteString            as B
import           Data.Data                  (Data, Typeable)

--------------------------------------------------------------------------------

data JSONError
  = ParserError Pa.ParsingError
  | ValueError String
  deriving (Show, Eq, Data, Typeable)

instance Exception JSONError

--------------------------------------------------------------------------------

decodeD
  :: (Monad m, P.Proxy p, Ae.FromJSON r)
  => ()
  -> P.EitherP JSONError (P.StateP [B.ByteString] p)
     () (Maybe B.ByteString) y' y m r
decodeD = \() -> do
    ev <- P.liftP . P.runEitherP $ Pa.parse Ae.json' ()
    case ev of
      Left e  -> P.throw (ParserError e)
      Right v -> do
          case Ae.fromJSON v of
            Ae.Error e   -> P.throw (ValueError e)
            Ae.Success r -> return r

