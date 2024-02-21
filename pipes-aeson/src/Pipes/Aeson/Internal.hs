{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}

-- | This module provides internal utilities and it is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Pipes.Aeson" module instead.
module Pipes.Aeson.Internal
  ( DecodingError(..)
  , consecutively
  , decodeL
  , loopL
  ) where
import           Control.Exception                (Exception)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Aeson                       as Ae
import qualified Data.Attoparsec.Types            as Attoparsec
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Internal         as B (isSpaceWord8)
import           Data.Data                        (Data, Typeable)
import           Data.Function                    (fix)
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

--------------------------------------------------------------------------------

-- | Consecutively parse 'a' elements from the given 'Producer' using the given
-- parser (such as 'Pipes.Aeson.decode' or 'Pipes.Aeson.parseValue'), skipping
-- any leading whitespace each time.
--
-- This 'Producer' runs until it either runs out of input or until a decoding
-- failure occurs, in which case it returns 'Left' with a 'DecodingError' and
-- a 'Producer' with any leftovers. You can use 'Pipes.Lift.exceptP' to turn the
-- 'Either' return value into an 'Control.Monad.Trans.ExceptT'
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

-- | Repeteadly try to parse raw JSON bytes into @a@ values, reporting any
-- 'I.DecodingError's downstream as they happen.
loopL
  :: (Monad m, Ae.FromJSON a)
  => Attoparsec.Parser B.ByteString Ae.Value
  -> (Pipes.Producer B.ByteString m r -> Pipes.Producer B.ByteString m r)
  -- ^ In case of 'AttoparsecError', this function will be called to modify
  -- the leftovers 'Pipes.Producer' before using it.
  --
  -- Ideally you will want to drop everything until the beginning of the next
  -- JSON element. This is easy to accomplish if there is a clear whitespace
  -- delimiter between the JSON elements, such as a newline (i.e.,
  -- @'Pipes.ByteString.drop' 1 . 'Pipes.ByteString.dropWhile' (/= 0xA)@).
  -- However, it can be hard to do correctly is there is no such delimiter.
  -- Skipping the first character (i.e., @'Pipes.ByteString.drop' 1@) should be
  -- sufficient in most cases, but not when parsing recursive data structures
  -- because you can accidentally parse a child in its parent's stead.
  --
  -- Notice that unless you advance the 'Pipes.Producer' somehow, 'loopL' will
  -- never terminate.
  -> Pipes.Producer B.ByteString m r
  -- ^ Raw JSON input.
  -> Pipes.Proxy x' x () (Either DecodingError (Int, a)) m r
{-# INLINABLE loopL #-}
loopL parser fp = fix $ \k p0 -> do
   (ye, p1) <- lift (S.runStateT (decodeL parser) p0)
   case ye of
      Just (Left e@AttoparsecError{}) -> do
         Pipes.yield (Left e)
         k (fp p1)
      Just ea -> Pipes.yield ea >> k p1
      Nothing -> lift (Pipes.next p1) >>= \case
         Left r -> pure r
         Right _ -> error "Pipes.Aeson.Internal.loopL: impossible"

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
