module TypeChecking.Binding where

import TypeChecking.Base
import Syntax.Binding
import Control.Monad.Except
--
-- freeChannels :: Process -> Set Chan
-- freeChannels (Call ) = freeChannels p `Set.union` freeChannels q
-- freeChannels (Compose x _ p q _) = freeChannels p `Set.union` freeChannels q

bindingCheck :: Process -> TCM ()
-- bindingCheck (Compose x _ p q _) = do

bindingCheck process = error $ show process
    -- throwError undefined
