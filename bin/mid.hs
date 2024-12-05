import MPlayer  ( myMain, parseOptions )

import Base1

-- monadio-plus ------------------------

import MonadIO.Base ( getArgs )

-- stdmain -----------------------------

import StdMain            ( stdMainNoDR )
import StdMain.UsageError ( UsageFPProcIOError )

--------------------------------------------------------------------------------

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "write essential stats for one or more video files"
      my_main = myMain @UsageFPProcIOError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)

-- that's all, folks! ----------------------------------------------------------
