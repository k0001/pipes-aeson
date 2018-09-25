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
import qualified Pipes.Parse                      as Pipes

--------------------------------------------------------------------------------

-- | An error while decoding a JSON value.
data DecodingError
  = AttoparsecError PA.ParsingError
    -- ^An @attoparsec@ error that happened while parsing the raw JSON string.
  | FromJSONError Ae.Value String
    -- ^An @aeson@ error that happened while trying to convert the given
    -- 'Data.Aeson.Value' to an 'Ae.FromJSON' instance, as reported by
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
  :: Monad m
  => Pipes.Parser B.ByteString m (Maybe (Either e a))
  -> Producer B.ByteString m r  -- ^Producer from which to draw raw input.
  -> Producer a m (Either (e, Producer B.ByteString m r) r)
consecutively parser = step where
    step p0 = do
      x <- lift $ nextSkipBlank p0
      case x of
         Left r -> return (Right r)
         Right (bs, p1) -> do
            (mea, p2) <- lift $ S.runStateT parser (yield bs >> p1)
            case mea of
               Just (Right a) -> yield a >> step p2
               Just (Left  e) -> return (Left (e, p2))
               Nothing -> error "Pipes.Aeson.Internal.consecutively: impossible"
{-# INLINABLE consecutively #-}


-- | Decodes a 'Ae.FromJSON' value from the underlying state using the given
-- 'Attoparsec.Parser' in order to obtain an 'Ae.Value' first.
--
-- It returns 'Nothing' if the underlying 'Producer' is exhausted, otherwise
-- it returns either the decoded entity or a 'I.DecodingError' in case of error.
decodeL
  :: (Monad m, Ae.FromJSON a)
  => Attoparsec.Parser B.ByteString Ae.Value
  -> Pipes.Parser B.ByteString m (Maybe (Either DecodingError (Int, a))) -- ^
decodeL parser = do
    mev <- PA.parseL parser
    return $ case mev of
       Nothing             -> Nothing
       Just (Left l)       -> Just (Left (AttoparsecError l))
       Just (Right (n, v)) -> case Ae.fromJSON v of
          Ae.Error e   -> Just (Left (FromJSONError v e))
          Ae.Success a -> Just (Right (n, a))
{-# INLINABLE decodeL #-}


--------------------------------------------------------------------------------
-- Internal stuff

-- | Like 'Pipes.next', except it skips leading whitespace and 'B.null' chunks.
nextSkipBlank
  :: (Monad m)
  => Producer B.ByteString m r
  -> m (Either r (B.ByteString, Producer B.ByteString m r))
nextSkipBlank = go where
    go p0 = do
      x <- next p0
      case x of
         Left  _      -> return x
         Right (a,p1) -> do
            let a' = B.dropWhile B.isSpaceWord8 a
            if B.null a' then go p1
                         else return (Right (a', p1))
{-# INLINABLE nextSkipBlank #-}
