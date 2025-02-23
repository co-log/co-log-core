{-# LANGUAGE CPP #-}

{- |
Module                  : Colog.Core.IO
Copyright               : (c) 2018-2020 Kowainik, 2021-2025 Co-Log
SPDX-License-Identifier : MPL-2.0
Maintainer              : Co-Log <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Introduces logging actions working in 'MonadIO'. These actions are very basic
and inefficient because they use the 'String' data type. If you don't want to
have extra dependencies and performance of logging is not the bottleneck of your
application, then these functions should be enough. Otherwise use functions from
the "Colog.Actions" module from the @co-log@ package.
-}

module Colog.Core.IO
       ( -- * 'String' actions
         logStringStdout
       , logStringStderr
       , logStringHandle
       , withLogStringFile

         -- * 'Show' actions
       , logPrint
       , logPrintStderr
       , logPrintHandle
       , withLogPrintFile

         -- * Various combinators
       , liftLogIO
       , logFlush
       ) where

import Colog.Core.Action      (LogAction (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup         ((<>))
import System.IO              (Handle, IOMode (AppendMode), hFlush, hPrint,
                               hPutStrLn, stderr, withFile)


{- $setup
>>> import Colog.Core.Action
-}

----------------------------------------------------------------------------
-- String
----------------------------------------------------------------------------

{- | Action that prints 'String' to stdout.
This action does not flush the output buffer.
If buffering mode is block buffering, the effect of this action can be delayed.

>>> logStringStdout <& "foo"
foo
-}
logStringStdout :: MonadIO m => LogAction m String
logStringStdout = LogAction (liftIO . putStrLn)
{-# INLINE logStringStdout #-}
{-# SPECIALIZE logStringStdout :: LogAction IO String #-}

{- | Action that prints 'String' to stderr.
This action does not flush the output buffer.
If buffering mode is block buffering, the effect of this action can be delayed.

>>> logStringStderr <& "foo"
foo
-}
logStringStderr :: MonadIO m => LogAction m String
logStringStderr = logStringHandle stderr
{-# INLINE logStringStderr #-}
{-# SPECIALIZE logStringStderr :: LogAction IO String #-}

{- | Action that prints 'String' to 'Handle'.
This action does not flush the output buffer.
If buffering mode is block buffering, the effect of this action can be delayed.

>>> logStringHandle stderr <& "foo"
foo
-}
logStringHandle :: MonadIO m => Handle -> LogAction m String
logStringHandle handle = LogAction $ liftIO . hPutStrLn handle
{-# INLINE logStringHandle #-}
{-# SPECIALIZE logStringHandle :: Handle -> LogAction IO String #-}

{- | Action that prints 'String' to file. Instead of returning 'LogAction' it's
implemented in continuation-passing style because it's more efficient to open
file only once at the start of the application and write to 'Handle' instead of
opening file each time we need to write to it.

Opens file in 'AppendMode'. Automatically flushes the output buffer.

#ifndef mingw32_HOST_OS

>>> logger action = action <& "foo"
>>> withLogStringFile "/dev/stdout" logger
foo

#endif
-}
withLogStringFile :: MonadIO m => FilePath -> (LogAction m String -> IO r) -> IO r
withLogStringFile path action = withFile path AppendMode $ \handle ->
  action (logStringHandle handle <> logFlush handle)
{-# INLINE withLogStringFile #-}
{-# SPECIALIZE withLogStringFile :: FilePath -> (LogAction IO String -> IO r) -> IO r #-}

----------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------

{- | Action that prints to stdout using 'Show'.
This action does not flush the output buffer.
If buffering mode is block buffering, the effect of this action can be delayed.

>>> logPrint <& 5
5
-}
logPrint :: forall a m . (Show a, MonadIO m) => LogAction m a
logPrint = LogAction $ liftIO . print
{-# INLINE logPrint #-}
{-# SPECIALIZE logPrint :: Show a => LogAction IO a #-}

{- | Action that prints to stderr using 'Show'.
This action does not flush the output buffer.
If buffering mode is block buffering, the effect of this action can be delayed.

>>> logPrintStderr <& 5
5
-}
logPrintStderr :: forall a m . (Show a, MonadIO m) => LogAction m a
logPrintStderr = logPrintHandle stderr
{-# INLINE logPrintStderr #-}
{-# SPECIALIZE logPrintStderr :: Show a => LogAction IO a #-}

{- | Action that prints to a 'Handle' using 'Show'.
This action does not flush the output buffer.
If buffering mode is block buffering, the effect of this action can be delayed.

>>> logPrintHandle stderr <& 5
5
-}
logPrintHandle :: forall a m . (Show a, MonadIO m) => Handle -> LogAction m a
logPrintHandle handle = LogAction $ liftIO . hPrint handle
{-# INLINE logPrintHandle #-}
{-# SPECIALIZE logPrintHandle :: Show a => Handle -> LogAction IO a #-}

{- | Action that prints to a file using 'Show'. See 'withLogStringFile' for details.
-}
withLogPrintFile
    :: forall a m r . (Show a, MonadIO m)
    => FilePath
    -> (LogAction m a -> IO r)
    -> IO r
withLogPrintFile path action = withFile path AppendMode $ \handle ->
  action (logPrintHandle handle <> logFlush handle)
{-# INLINE withLogPrintFile #-}
{-# SPECIALIZE withLogPrintFile :: Show a => FilePath -> (LogAction IO a -> IO r) -> IO r #-}

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

{- | Lifts a LogAction over IO into a more general Monad.

>>> logToStdout = LogAction putStrLn
>>> liftLogIO logToStdout <& "foo"
foo
-}
liftLogIO :: MonadIO m => LogAction IO msg -> LogAction m msg
liftLogIO (LogAction action) = LogAction (liftIO . action)
{-# INLINE liftLogIO #-}

{- | This action can be used in combination with other actions to flush
a handle every time you log anything.

@since 0.3.0.0
-}
logFlush :: MonadIO m => Handle -> LogAction m a
logFlush handle = LogAction $ const $ liftIO $ hFlush handle
{-# INLINE logFlush #-}
{-# SPECIALIZE logFlush :: Handle -> LogAction IO a #-}
