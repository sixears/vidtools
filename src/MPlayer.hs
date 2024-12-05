{-# LANGUAGE UnicodeSyntax #-}
module MPlayer
  ( myMain, parseOptions
  ) where

--------------------------------------------------------------------------------

import Base1

import Prelude ( Float, fromRational )

-- base --------------------------------

import Data.Function ( flip )
import Data.List     ( concatMap, sortOn )
import Data.Maybe    ( catMaybes )
import Text.Read     ( Read, readEither )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- data-textual ------------------------

import Data.Textual qualified

-- duration ----------------------------

import Duration ( Duration, asSeconds )

-- env-plus ----------------------------

import Env.Types ( ә, ӭ )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile, absfile )
import FPath.Error.FPathError ( AsFPathError )
import FPath.File             ( File )
import FPath.Parseable        ( readM )

-- fstat -------------------------------

import FStat qualified

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, Severity(Informational) )

-- log-plus ----------------------------

import Log ( Log, debugT )

-- mockio-log --------------------------

import MockIO.DoMock      ( DoMock(NoMock) )
import MockIO.MockIOClass ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.FStat   ( stat )
import MockIO.Process ( ꙩ )

-- monadio-plus ------------------------

import MonadIO                       ( say, warn )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.FPath                 ( pResolve )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⫤) )

-- mtl ---------------------------------

import Control.Monad.Reader ( runReaderT )

-- optparse-applicative ----------------

import Options.Applicative.Builder ( argument, flag, help, long, metavar, str )
import Options.Applicative.Types   ( Parser )

-- optparse-plus -----------------------

import OptParsePlus ( parseNE )

-- stdmain -----------------------------

import StdMain            ( stdMainNoDR )
import StdMain.UsageError ( UsageFPProcIOError )

-- text --------------------------------

import Data.Text ( breakOn, drop, intercalate, isPrefixOf, pack, unpack )

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual'), parseTextual )

------------------------------------------------------------
--                     local imports
------------------------------------------------------------

import MPlayer.Paths qualified as Paths

--------------------------------------------------------------------------------

data Presentation = Human | Tabs
data Mode = ModeRaw
          | ModeParsed Presentation

data Options = Options { _mode   :: Mode
                       , _inputs :: NonEmpty 𝕋
                       }

----------------------------------------

inputs ∷ ∀ ε μ . (AsIOError ε, AsFPathError ε, MonadError ε μ,
                  HasCallStack, MonadIO μ) ⇒
         Options → μ (NonEmpty AbsFile)
inputs o = sequence $ pResolve @AbsFile ⊳ _inputs o

----------------------------------------

mode ∷ Lens' Options Mode
mode = lens _mode (\ o is → o { _mode = is })

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ ( flag (ModeParsed Human) ModeRaw
                   (long "raw" ⊕ help "output all ID_ tags")
            ∤ flag (ModeParsed Human) (ModeParsed Tabs)
                   (long "tabs" ⊕ help "output tab-delimited"))
          ⊵ parseNE (argument str (metavar "FILENAME"))

------------------------------------------------------------

data FileData = FileData { _len    :: Duration
                         , _width  :: ℕ
                         , _height :: ℕ
                         , _size   :: Word64
                         }

fd_len ∷ Lens' FileData Duration
fd_len = lens _len (\ o l → o { _len = l })

fd_size ∷ Lens' FileData Word64
fd_size = lens _size (\ o s → o { _size = s })

fd_width ∷ Lens' FileData ℕ
fd_width = lens _width (\ o w → o { _width = w })

fd_height ∷ Lens' FileData ℕ
fd_height = lens _height (\ o h → o { _height = h })

----------------------------------------

parseMIdentify ∷ 𝕄 FStat.FStat → Map.Map 𝕋 𝕋 → 𝔼 𝕋 FileData
parseMIdentify st identifiers = do
  let readEitherT ∷ Read α ⇒ 𝕊 → 𝕋 → 𝔼 𝕋 α
      readEitherT typ s =
        case readEither (unpack s) of
          𝕽 x → 𝕽 $ x
          𝕷 e → 𝕷 $ ([fmt|failed to parse %t as %s: %s|] s typ e)
      get ∷ 𝕋 → (𝕋 → 𝔼 𝕋 α) → 𝔼 𝕋 α
      get name f = maybe (𝕷 $ [fmt|no %t found|] name)
                         f (identifiers ⫤ name)

  l ← get "ID_LENGTH" (parseTextual ∘ (⊕"s"))
  w ← get "ID_VIDEO_WIDTH" (readEitherT "ℕ")
  h ← get "ID_VIDEO_HEIGHT" (readEitherT "ℕ")
  z ← maybe (𝕷 "empty stat") (𝕽 ∘ FStat.size) st
  return $ FileData l w h z

----------------------------------------

format ∷ Presentation → AbsFile → FileData → 𝕋
format presentation file_name file_data =
  let len    = file_data ⊣ fd_len
      width  = file_data ⊣ fd_width
      height = file_data ⊣ fd_height
      size   = file_data ⊣ fd_size
  in  case presentation of
        Human → [fmt|%T\t%,10d\t%10t\t%8T|]
                file_name size ([fmt|%dx%d|] width height) len
        Tabs  → intercalate "\t" [ toText file_name
                                 , toText size
                                 , pack $ show width
                                 , pack $ show height
                                 , toText (fromRational @Float $ len ⊣ asSeconds)
                                 ]

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
          AsProcExitError ε, Printable ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain opts = flip runReaderT NoMock $ do
  ins ∷ NonEmpty AbsFile ← inputs opts
  xs ∷ NonEmpty (𝕄 𝕋) ← forM ins $ \ input → do
    let args = [ "-vo", "null", "-ao", "null", "-frames", "0", "-identify"
               , "-lavfdopts", "analyzeduration=10", "-lavfdopts", "probesize=10000000"
               , toText input ]
        parseLine l = if "ID_" `isPrefixOf` l
                      then case breakOn "=" l of
                             (_,"") → []
                             (k,v)  → [(k,drop 1 v)]
                      else []
        printRaw (k,v) = say  $ [fmtT|%t\t%t|] k v

    -- this will error out in case of non-zero exit, so no need to
    -- bind the exit value to a var
    (_,(stdout∷[𝕋],stderr∷[𝕋])) ← ꙩ (Paths.mplayer,args, [ӭ $ ә"HOME"])
    forM_ stderr debugT

    let m_identifiers = Map.fromList $ concatMap parseLine stdout

    case opts ⊣ mode of
      ModeRaw → do forM_ (sortOn fst $ Map.toList m_identifiers) printRaw
                   return 𝕹
      ModeParsed presentation → do
        st ← stat Informational 𝕹 input NoMock
        case parseMIdentify st m_identifiers of
          𝕽 file_data → do say $ format presentation input file_data
                           return 𝕹
          𝕷 e         → return $ 𝕵 ([fmtT|%T: %t|] input e)

  case catMaybes $ toList xs of
    []  → return 0
    xs' → forM_ xs' (\ x → warn x) ⪼ return 10

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "write essential stats for one or more video files"
      my_main = myMain @UsageFPProcIOError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)

-- that's all, folks! ----------------------------------------------------------
