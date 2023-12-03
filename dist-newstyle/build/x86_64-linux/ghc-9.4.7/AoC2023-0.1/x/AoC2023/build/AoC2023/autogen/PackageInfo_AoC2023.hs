{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_AoC2023 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "AoC2023"
version :: Version
version = Version [0,1] []

synopsis :: String
synopsis = "Advent of Code 2023 solutions"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
