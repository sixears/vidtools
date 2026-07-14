{-# LANGUAGE UnicodeSyntax #-}
module Video.Mid
  ( main
  ) where

--------------------------------------------------------------------------------

import Base1

import Prelude ( Float, fromRational )

-- base --------------------------------

import Data.List     ( sortOn )
import Data.Maybe    ( catMaybes )
import Text.Read     ( Read, readEither )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- duration ----------------------------

import Duration ( Duration, asSeconds )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.Error.FPathError ( AsFPathError )

-- fstat -------------------------------

import FStat qualified

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, Severity(Informational) )

-- log-plus ----------------------------

import Log ( Log )

-- mockio-log --------------------------

import MockIO.DoMock      ( DoMock(NoMock) )
import MockIO.MockIOClass ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.FStat ( stat )

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
import StdMain.UsageError ( UsageFPProcIOTPError )

-- text --------------------------------

import Data.Text ( intercalate, pack, unpack )

-- textual-plus ------------------------

import TextualPlus                          ( parseTextual )
import TextualPlus.Error.TextualParseError  ( AsTextualParseError )

------------------------------------------------------------
--                     local imports
------------------------------------------------------------

import Video.MIdentify ( midentify )

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
          𝓡 x → 𝓡 $ x
          𝓛 e → 𝓛 $ ([fmt|failed to parse %t as %s: %s|] s typ e)
      get ∷ 𝕋 → (𝕋 → 𝔼 𝕋 α) → 𝔼 𝕋 α
      get name f = maybe (𝓛 $ [fmt|no %t found|] name)
                         f (identifiers ⫤ name)

  l ← get "LENGTH" (parseTextual ∘ (⊕"s"))
  w ← get "VIDEO_WIDTH" (readEitherT "ℕ")
  h ← get "VIDEO_HEIGHT" (readEitherT "ℕ")
  z ← maybe (𝓛 "empty stat") (𝓡 ∘ FStat.size) st
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
          AsProcExitError ε, AsTextualParseError ε, Printable ε) =>
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain opts = flip runReaderT NoMock $ do
  ins ∷ NonEmpty AbsFile ← inputs opts

  warnings ∷ NonEmpty (𝕄 𝕋) ← forM ins $ \ input → do
    (_, m_identifiers) ← midentify input
    case opts ⊣ mode of
      ModeRaw → do let printRaw (k,v) = say  $ [fmtT|%t\t%t|] k v
                   forM_ (sortOn fst $ Map.toList m_identifiers) printRaw
                   return 𝓝
      ModeParsed presentation → do
        st ← stat Informational 𝓝 input NoMock
        -- say $ intercalate "," $ Map.keys m_identifiers
        case parseMIdentify st m_identifiers of
          𝓡 file_data → do say $ format presentation input file_data
                           return 𝓝
          𝓛 e         → return $ 𝓙 ([fmtT|%T: %t|] input e)

  case catMaybes $ toList warnings of
    [] → return 0
    ws → forM_ ws warn ⪼ return 10

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "write essential stats for one or more video files"
      my_main = myMain @UsageFPProcIOTPError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)

-- that's all, folks! ----------------------------------------------------------
