{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides internal utilities and it is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Pipes.Aeson" module instead.
module Pipes.Aeson.Internal
  ( DecodingError(..)
  , consecutively
  , skipSpace
  , fromLazy
  ) where

import           Control.Exception                (Exception)
import qualified Control.Monad.Trans.Error        as E
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Aeson                       as Ae
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Internal    as BLI
import qualified Data.Char                        as Char
import           Data.Data                        (Data, Typeable)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import qualified Pipes.Parse                      as Pp
import qualified Pipes.Lift                       as P

--------------------------------------------------------------------------------

-- | An error while decoding a JSON value.
data DecodingError
  = ParserError PA.ParsingError
    -- ^An Attoparsec error that happened while parsing the raw JSON string.
  | ValueError String
    -- ^An Aeson error that happened while trying to convert a
    -- 'Data.Aeson.Value' to  an 'A.FromJSON' instance, as reported by
    -- 'Data.Aeson.Error'.
  deriving (Show, Eq, Data, Typeable)

instance Exception DecodingError
instance E.Error DecodingError
instance Monad m => E.Error (DecodingError, Producer a m r)

--------------------------------------------------------------------------------

-- | Consecutively parse 'b' elements from the given 'Producer' using the given
-- parser (such as 'Pipes.Aeson.decode' or 'Pipes.Aeson.parseValue'), skipping
-- any leading whitespace each time.
--
-- This 'Producer' runs until it either runs out of input or until a decoding
-- failure occurs, in which case it returns 'Left' with a 'I.DecodingError' and
-- a 'Producer' with any leftovers. You can use 'P.errorP' to turn the 'Either'
-- return value into an 'Control.Monad.Trans.Error.ErrorT' monad transformer.
consecutively
  :: (Monad m, Ae.FromJSON b)
  => S.StateT (Producer B.ByteString m r) m (Either DecodingError (Int, b))
  -> Producer B.ByteString m r  -- ^Producer from which to draw JSON.
  -> Producer (Int, b) m
              (Either (DecodingError, Producer B.ByteString m r) r)
consecutively parser = \src -> do
    (er, src') <- P.runStateP src prod
    return $ case er of
      Left  e  -> Left  (e, src')
      Right r  -> Right r
  where
    prod = do
        eof <- lift (skipSpace >> PA.isEndOfParserInput)
        if eof
          then do
            ra <- lift Pp.draw
            case ra of
              Left  r -> return (Right r)
              Right _ -> error "Pipes.Aeson.parseMany: impossible!!"
          else do
            eb <- lift parser
            case eb of
              Left  e -> return (Left e)
              Right b -> yield b >> prod
{-# INLINABLE consecutively #-}

--------------------------------------------------------------------------------

-- XXX we define the following proxies here until 'pipes-bytestring' is released

-- | Consumes and discards leading 'I.ParserInput' characters from upstream as
-- long as the given predicate holds 'True'.
skipSpace :: Monad m => S.StateT (Producer B.ByteString m r) m ()
skipSpace = do
    ma <- Pp.draw
    case ma of
      Left  _ -> return ()
      Right a -> do
        let a' = B.dropWhile Char.isSpace a
        if B.null a'
           then skipSpace
           else Pp.unDraw a'
{-# INLINABLE skipSpace #-}

-- Sends each of the 'BLI.ByteString''s strict chunks downstream.
fromLazy :: Monad m => BLI.ByteString -> Producer' B.ByteString m ()
fromLazy = BLI.foldrChunks (\e a -> yield e >> a) (return ())
{-# INLINABLE fromLazy #-}
