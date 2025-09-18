module Caide.Types.ProgrammingLanguage(
      ProgrammingLanguage(..)
) where

import Caide.Monad (CaideIO)
import Caide.Types (ProblemID)

-- | The type encapsulating functions required to support particular target
--   programming language.
data ProgrammingLanguage = ProgrammingLanguage
    { generateScaffold   :: ProblemID -> CaideIO ()
    , inlineCode         :: ProblemID -> CaideIO ()
    }

