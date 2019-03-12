module Runtime where


-- import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
-- import Syntax.Abstract (Session, Type(..), TypeVar(..))
import Syntax.Abstract

type Channal = TermName

-- reduce :: Channal -> Process -> Process -> Process
-- reduce chan (Output x y p q)  (Input x' y' r) = Compose y Nothing p (Compose x Nothing q r)
-- reduce chan (SelectL x p)     (Choice x' q r) = Compose x Nothing p q
-- reduce chan (SelectR x p)     (Choice x' q r) = Compose x Nothing p r
-- reduce chan (Acc x y p)       (Req x' y' q)   = Compose y Nothing p q
-- reduce chan (Acc x y p)       (Req x' y' q)   = Compose y Nothing p q
-- reduce chan _ _ = undefined
