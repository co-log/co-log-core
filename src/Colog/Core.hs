{- |
Module                  : Colog.Core
Copyright               : (c) 2018-2020 Kowainik, 2021-2025 Co-Log
SPDX-License-Identifier : MPL-2.0
Maintainer              : Co-Log <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Exports all core functionality. @co-log-core@ is a lightweight package that
defines only core data type and various combinators to work with it.

Fundamentals of @co-log-core@ are based on the following data type:

@
__newtype__ LogAction m msg = LogAction
    { unLogAction :: msg -> m ()
    }
@

This data type provides extremely composable and flexible interface by having
many instances of the standard algebraic data types.

The package has the following structure:

* __"Colog.Core.Action":__ definition of the main data type and its combinators.
* __"Colog.Core.Class":__ 'HasLog' typeclass that describes how different values
  (e.g. application environment) can store and modify 'LogAction'.
* __"Colog.Core.IO":__ basic loggers that work with 'Control.Monad.IO.Class.MonadIO' and 'String'.
* __"Colog.Core.Severity":__ logger severity.
-}

module Colog.Core
       ( module Colog.Core.Action
       , module Colog.Core.Class
       , module Colog.Core.IO
       , module Colog.Core.Severity
       ) where

import Colog.Core.Action
import Colog.Core.Class
import Colog.Core.IO
import Colog.Core.Severity
