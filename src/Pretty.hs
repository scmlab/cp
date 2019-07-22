{-# LANGUAGE OverloadedStrings                  #-}

module Pretty
  ( module Data.Text.Prettyprint.Doc
  , module Data.Text.Prettyprint.Doc.Render.Terminal
  ) where

import Pretty.Syntax.Binding ()
import Pretty.Syntax.Concrete ()
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
