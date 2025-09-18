module Caide.Types.Builder(
      BuilderResult(..)
    , Builder
) where

import Caide.Monad (CaideIO)
import Caide.Types (ProblemID)

-- | Builder is responsible for building the code and running test program
type Builder = ProblemID -> CaideIO BuilderResult

-- | 'Builder' result
data BuilderResult = BuildFailed  -- ^ Build failed or program under test exited unexpectedly
                   | TestsFailed  -- ^ Build succeeded, tests have been evaluated and failed
                   | NoEvalTests  -- ^ Build succeeded, tests have not been evaluated
                   | TestsPassed  -- ^ Tests succeeded

