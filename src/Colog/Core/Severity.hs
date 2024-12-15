{-# LANGUAGE PatternSynonyms #-}

{- |
Module                  : Colog.Core.Severity
Copyright               : (c) 2018-2020 Kowainik, 2021-2025 Co-Log
SPDX-License-Identifier : MPL-2.0
Maintainer              : Co-Log <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module introduces 'Severity' data type for expressing how severe the
message is. Also, it contains useful functions and patterns for work with 'Severity'.


 +-----------+---------+-----------------------------------------+-----------------------------+
 | Severity  | Pattern | Meaning                                 | Example                     |
 +===========+=========+=========================================+=============================+
 | 'Debug'   | 'D'     | Information useful for debug purposes   | Internal function call logs |
 +-----------+---------+-----------------------------------------+-----------------------------+
 | 'Info'    | 'I'     | Normal operational information          | Finish file uploading       |
 +-----------+---------+-----------------------------------------+-----------------------------+
 | 'Warning' | 'W'     | General warnings, non-critical failures | Image load error            |
 +-----------+---------+-----------------------------------------+-----------------------------+
 | 'Error'   | 'E'     | General errors/severe errors            | Could not connect to the DB |
 +-----------+---------+-----------------------------------------+-----------------------------+
-}

module Colog.Core.Severity
       ( Severity (..)
         -- ** Patterns
         -- $pattern
       , pattern D
       , pattern I
       , pattern W
       , pattern E
       , filterBySeverity
       , WithSeverity (..)
       , mapSeverity
       ) where

import Data.Ix (Ix)

import Colog.Core.Action (LogAction (..), cfilter)


-- | Severity for the log messages.
data Severity
    {- | Information useful for debug purposes.

    E.g. output of the function that is important for the internal development,
    not for users. Like, the result of SQL query.
    -}
    = Debug
    {- | Normal operational information.

    E.g. describing general steps: starting application, finished downloading.
    -}
    | Info
    {- | General warnings, non-critical failures.

    E.g. couldn't download icon from some service to display.
    -}
    | Warning
    {- | General errors/severe errors.

    E.g. exceptional situations: couldn't syncronize accounts.
    -}
    | Error
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Ix)

{- $pattern
Instead of using full names of the constructors you can instead use one-letter
patterns. To do so you can import and use the pattern:

@
__import__ Colog (__pattern__ D)

example :: WithLog env Message m => m ()
example = log D "I'm using severity pattern"
@

Moreover, you could use patterns when pattern-matching on severity

@
errorToStderr :: 'Severity' -> IO ()
errorToStderr E = hputStrLn stderr "Error severity"
errorToStderr _ = putStrLn "Something else"
@
-}

pattern D, I, W, E :: Severity
pattern D <- Debug   where D = Debug
pattern I <- Info    where I = Info
pattern W <- Warning where W = Warning
pattern E <- Error   where E = Error
{-# COMPLETE D, I, W, E #-}

-- | Filters messages by the given 'Severity'.
filterBySeverity
    :: Applicative m
    => Severity
    -> (a -> Severity)
    -> LogAction m a
    -> LogAction m a
filterBySeverity s fs = cfilter (\a -> fs a >= s)
{-# INLINE filterBySeverity #-}

-- Note: the order of the fields here is to allow the constructor to be used infix.
{-| A message tagged with a 'Severity'.
 
It is common to want to log various types of messages tagged with a severity. 
'WithSeverity' provides a standard way to do so while allowing the messages to be processed independently of the severity.

It is easy to 'cmap' over a 'LogAction m (WithSeverity a)', or to filter based on the severity.

@
logSomething :: 'LogAction' m ('WithSeverity' 'String') -> m ()
logSomething logger = logger <& "hello" \`WithSeverity\` 'Info'

cmap' :: (b -> a) -> 'LogAction' m ('WithSeverity' a) -> 'LogAction' m ('WithSeverity' b)
cmap' f action = 'cmap' ('fmap' f) action

filterBySeverity' :: ('Applicative' m) => 'Severity' -> 'LogAction' m ('WithSeverity' a) -> 'LogAction' m ('WithSeverity' a)
filterBySeverity' threshold action = 'filterBySeverity' threshold 'getSeverity' action
@

@since 0.3.1.0
-}
data WithSeverity msg = WithSeverity { getMsg :: msg , getSeverity :: Severity }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | Map the given function over the severity of a 'WithSeverity'.
 
This can be useful to operate generically over the severity, for example:

@
suppressErrors :: 'LogAction' m ('WithSeverity' msg) -> 'LogAction' m ('WithSeverity' msg)
suppressErrors = 'cmap' ('mapSeverity' (\s -> if s == 'Error' then 'Warning' else s))
@

@since 0.3.1.0
-}
mapSeverity :: (Severity -> Severity) -> WithSeverity msg -> WithSeverity msg
mapSeverity f (WithSeverity msg sev) = WithSeverity msg (f sev)
{-# INLINE mapSeverity #-}
