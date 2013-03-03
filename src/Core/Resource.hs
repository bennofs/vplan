{-# LANGUAGE FlexibleContexts #-}
module Core.Resource where

import qualified Data.ByteString as B
import qualified Filesystem.Path.CurrentOS as FP

data Resource = File FP.FilePath | HTTP B.ByteString
