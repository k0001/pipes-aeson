{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}

-- | This module provides internal utilities and it is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Pipes.Aeson" module instead.
module Pipes.Aeson.Internal
  ( DecodingError(..)
  , consecutively
  , decodeL
  ) where
import           Control.Exception                (Exception)
import           Control.Monad.Trans.Error        (Error)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Aeson                       as Ae
import qualified Data.Attoparsec.Types            as Attoparsec
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Internal         as B (isSpaceWord8)
import           Data.Data                        (Data, Typeable)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Parse                      as Pipes

--------------------------------------------------------------------------------

-- | An error while decoding a JSON value.
data DecodingError
  = AttoparsecError PA.ParsingError
    -- ^An @attoparsec@ error that happened while parsing the raw JSON string.
  | FromJSONError String
    -- ^An @aeson@ error that happened while trying to convert a
    -- 'Data.Aeson.Value' to an 'A.FromJSON' instance, as reported by
    -- 'Data.Aeson.Error'.
  deriving (Show, Eq, Data, Typeable)

instance Exception DecodingError
instance Error     DecodingError

-- | This instance allows using 'Pipes.Lift.errorP' with 'Pipes.Aeson.decoded'
-- and 'Pipes.Aeson.decodedL'
instance Error (DecodingError, Producer a m r)

--------------------------------------------------------------------------------

-- | Consecutively parse 'a' elements from the given 'Producer' using the given
-- parser (such as 'Pipes.Aeson.decode' or 'Pipes.Aeson.parseValue'), skipping
-- any leading whitespace each time.
--
-- This 'Producer' runs until it either runs out of input or until a decoding
-- failure occurs, in which case it returns 'Left' with a 'DecodingError' and
-- a 'Producer' with any leftovers. You can use 'Pipes.Lift.errorP' to turn the
-- 'Either' return value into an 'Control.Monad.Trans.Error.ErrorT'
-- monad transformer.
consecutively
  :: (Monad m)
  => Pipes.Parser B.ByteString m (Either e a)
  -> Producer B.ByteString m r  -- ^Producer from which to draw raw input.
  -> Producer a m (Either (e, Producer B.ByteString m r) r)
consecutively parser = step where
    step p0 = do
      x <- lift $ next (p0 >-> PB.dropWhile B.isSpaceWord8)
      case x of
         Left r -> return (Right r)
         Right (bs, p1) -> do
            (ea, p2) <- lift $ S.runStateT parser (yield bs >> p1)
            case ea of
               Left  e -> return (Left (e, p2))
               Right a -> yield a >> step p2
{-# INLINABLE consecutively #-}


-- | Decodes a 'Ae.FromJSON' value from the underlying state using the given
-- 'Attoparsec.Parser' in order to obtain an 'Ae.Value' first.
--
-- Returns either the decoded entitiy, or a 'I.DecodingError' in case of error.
--
-- /Do not/ use this function if the underlying 'Producer' has leading empty
-- chunks or whitespace, otherwise you may get unexpected parsing errors.
decodeL
  :: (Monad m, Ae.FromJSON a)
  => Attoparsec.Parser B.ByteString Ae.Value
  -> Pipes.Parser B.ByteString m (Either DecodingError (Int, a)) -- ^
decodeL parser = do
    ev <- PA.parseL parser
    return (case ev of
      Left  e      -> Left (AttoparsecError e)
      Right (n, v) -> case Ae.fromJSON v of
        Ae.Error e   -> Left (FromJSONError e)
        Ae.Success a -> Right (n, a))
{-# INLINABLE decodeL #-}
