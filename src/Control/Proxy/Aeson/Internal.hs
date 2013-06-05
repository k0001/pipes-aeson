-- | This module provides internal utilities and it is likely
-- to be modified in backwards-incompatible ways in the future.
--
-- Use the stable API exported by the "Control.Proxy.Aeson" module instead.
module Control.Proxy.Aeson.Internal
  ( skipSpace
  , fromLazy
  ) where

import qualified Control.Proxy                 as P
import qualified Control.Proxy.Parse           as Pp
import qualified Control.Proxy.Trans.State     as P
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Char                     as Char
import           Data.Function                 (fix)

--------------------------------------------------------------------------------
-- XXX we define the following proxies here until 'pipes-bytestring' is released

-- | Consumes and discards leading 'I.ParserInput' characters from upstream as
-- long as the given predicate holds 'True'.
skipSpace
  :: (Monad m, P.Proxy p)
  => P.StateP [B.ByteString] p () (Maybe B.ByteString) y' y m ()
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
fromLazy
  :: (Monad m, P.Proxy p)
  => BLI.ByteString -> p x' x y' B.ByteString m ()
fromLazy =
    P.runIdentityP . BLI.foldrChunks (\e a -> P.respond e >> a) (return ())
{-# INLINABLE fromLazy #-}