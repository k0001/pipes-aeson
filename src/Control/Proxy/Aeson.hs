{-# LANGUAGE DeriveDataTypeable #-}

-- | This module allows you to encode and decode JSON values flowing downstream
-- through Pipes streams, possibly interleaving other stream effects.
--
-- This module builds on top of the @pipes-parse@ package and assumes
-- you have read "Control.Proxy.Parse.Tutorial".

module Control.Proxy.Aeson
  ( -- * Decoding
    -- $decoding
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
import           Data.Data                  (Data, Typeable)

--------------------------------------------------------------------------------

-- | An error while decoding a JSON value.
data DecodingError
  = ParserError PA.ParsingError
    -- ^An Attoparsec error that happened while parsing the raw JSON string.
  | ValueError String
    -- ^An Aeson error that happened while trying to convert a 'Ae.Value' to
    --  an 'A.FromJSON' instance, as reported by 'Ae.Error'.
  deriving (Show, Eq, Data, Typeable)

instance Exception DecodingError

--------------------------------------------------------------------------------
-- $decoding
--
-- There are different JSON decoding facilities exported by this module, and
-- choosing between them is easy: If you need to interleave JSON decoding
-- with other stream effects you must use 'decode', otherwise you may use the
-- simpler 'decodeD'.
--
-- These proxies use the 'P.EitherP' proxy transformer to report decoding
-- errors, you might use any of the facilities exported by
-- "Control.Proxy.Trans.Either" to recover from them.


-- | Decodes one JSON value flowing downstream.
--
-- * This proxy is meant to be composed in the 'P.request' category.
--
-- * In case of decoding errors, a 'DecodingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * /Do not/ use this proxy if 'PA.isEndOfNonWhitespaceParserInput' returns
-- 'True', otherwise you may get unexpected boundary errors.
--
-- Here is an example parsing loop that allows interleaving stream effects
-- together with 'decode':
--
-- > loop = do
-- >     eof <- liftP isEndOfNonWhitespaceParserInput
-- >     unless eof $ do
-- >         -- 1. Possibly perform stream some effects here.
-- >         -- 2. Decode one JSON element from the stream.
-- >         exampleElement <- decode
-- >         -- 3. Do something with exampleElement and possibly perform
-- >         --    some more stream effects.
-- >         -- 4. Start all over again.
-- >         loop
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


-- | Decodes consecutive JSON values flowing downstream until
-- 'PA.isEndOfNonWhitespaceParserInput'.
--
-- * This proxy is meant to be composed in the 'P.pull' category.
--
-- * In case of decoding errors, a 'DecodingError' exception is thrown in
-- the 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * Empty input chunks flowing downstream and whitespace in between JSON
-- values will be discarded.
decodeD
  :: (Monad m, P.Proxy p, Ae.FromJSON b)
  => ()
  -> P.Pipe (P.EitherP DecodingError (P.StateP [B.ByteString] p))
     (Maybe B.ByteString) b m ()
decodeD = \() -> loop
  where
    loop = do
        eof <- P.liftP PA.isEndOfNonWhitespaceParserInput
        unless eof $ do
          () <- P.respond =<< decode
          loop
{-# INLINABLE decodeD #-}

