{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides internal utilities and it is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Pipes.Aeson" module instead.
module Pipes.Aeson.Internal
  ( DecodingError(..)
  , skipSpace
  , fromLazy
  ) where

import           Control.Exception                (Exception)
import qualified Control.Monad.Trans.Error        as E
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Internal    as BLI
import qualified Data.Char                        as Char
import           Data.Data                        (Data, Typeable)
import           Data.Function                    (fix)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import qualified Pipes.Parse                      as Pp

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
instance E.Error   DecodingError

--------------------------------------------------------------------------------

-- XXX we define the following proxies here until 'pipes-bytestring' is released

-- | Consumes and discards leading 'I.ParserInput' characters from upstream as
-- long as the given predicate holds 'True'.
skipSpace
  :: Monad m => Client Pp.Draw (Maybe B.ByteString) (StateT [B.ByteString] m) ()
skipSpace = fix $ \loop -> do
    ma <- Pp.draw
    case ma of
      Nothing -> return ()
      Just a  -> do
        let a' = B.dropWhile Char.isSpace a
        if B.null a'
           then loop
           else Pp.unDraw a'
{-# INLINABLE skipSpace #-}

-- Sends each of the 'BLI.ByteString''s strict chunks downstream.
fromLazy :: Monad m => BLI.ByteString -> Server y' B.ByteString m ()
fromLazy = BLI.foldrChunks (\e a -> respond e >> a) (return ())
{-# INLINABLE fromLazy #-}
