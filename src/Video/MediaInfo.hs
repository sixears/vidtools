{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Video.MediaInfo
  ( MediaInfo {- , filename -} )
where

import Base1
import Prelude ( Bounded, Integral, Num, error, fail, toRational )

-- aeson -------------------------------

import Data.Aeson         ( FromJSON( parseJSON ),
                            Value( Bool, Number, Object, String ), (.:),
                            eitherDecode, eitherDecodeStrict, withArray,
                            withObject, withText
                          )
import Data.Aeson.KeyMap  ( (!?) )

-- aeson-plus --------------------------

import Data.Aeson.Error  ( AsAesonError, throwAsAesonError )

-- base --------------------------------

import Data.Bool    ( bool )
import Data.List    ( isInfixOf )
import Data.Monoid  ( Monoid )
import Data.Ratio   ( Ratio, (%), denominator, numerator )
import Text.Read    ( Read, readEither )

-- base-unicode-symbols ----------------

import Prelude.Unicode  ( ℚ )

-- duration ----------------------------

import Duration  ( Duration( SECS ) )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask )

-- fpath -------------------------------

import FPath.AbsDir            ( absdir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( FileAs )
import FPath.Parseable         ( __parse__ )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( relfile )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog )

-- mockio-log --------------------------

import MockIO.DoMock      ( DoMock( NoMock ), HasDoMock )
import MockIO.Log         ( logit )
import MockIO.MockIOClass ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process  ( ꙩ )

-- monadio-plus ------------------------

import MonadIO                        ( say )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )

-- mtl -----------------------

import Control.Monad.Reader  ( MonadReader, runReaderT )

-- scientific --------------------------

import Data.Scientific  ( toBoundedInteger )

-- stdmain -----------------------------

import StdMain.UsageError ( UsageParseAesonFPPIOError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, assertEqual, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertLeft, assertRight, runTestsP, runTestsReplay,
                    runTestTree )

-- text --------------------------------

import Data.Text  qualified as  T

import Data.Text.Read  ( rational )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- vector ------------------------------

import Data.Vector  ( Vector )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MediaInfo.Paths qualified as Paths

import Video.MediaInfo.TestData  ( t0, t1, t2, t3, t4, t5, t6 )

--------------------------------------------------------------------------------

newtype Num α => MWord α = MWord α  deriving  (Eq, Num, Show)

instance (Bounded α, Integral α, Read α) => FromJSON (MWord α) where
  parseJSON (Number n) =
    case toBoundedInteger n of
      𝓙 i → return $ MWord i
      𝓝   → fail $ [fmt|failed parsing MWord as Number: %w|] n
  parseJSON (String s) =
    case readEither (T.unpack s) of
      𝓡 i → return $ MWord i
      𝓛 e → fail $ [fmt|failed parsing MWord as Text: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for MWord: '%w'|] v

------------------------------------------------------------

newtype MDuration = MDuration Duration  deriving  (Eq, Show)

instance FromJSON MDuration where
  parseJSON (Number n) = return ∘ MDuration ∘ SECS $ toRational n
  parseJSON (String s) =
    case (second (first SECS) ⊳ rational) s of
      𝓡 (d,_) → pure $ MDuration d
      𝓛  e    → fail $ [fmt|failed to parse Duration: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for MDuration: '%w'|] v

----------------------------------------

msecs ∷ ℚ → MDuration
msecs = MDuration ∘ SECS

------------------------------------------------------------

newtype Num α => MRatio α = MRatio (Ratio α)  deriving  (Eq, Num, Show)

instance (Integral α, Num α, Read α) => FromJSON (MRatio α) where
  parseJSON (Number n) =
    let r = toRational n
    in  pure ∘ MRatio $ (fromIntegral $ numerator r)
                      % (fromIntegral $ denominator r)
  parseJSON (String s) =
    case rational s of
      𝓡 (d,_) → pure $ MRatio d
      𝓛  e    → fail $ [fmt|failed to parse Ratio: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for MRatio: '%w'|] v

------------------------------------------------------------

newtype Num α => MRatioN α = MRatioN (Ratio α)  deriving  (Eq, Num, Show)

instance (Integral α, Num α, Read α) => FromJSON (MRatioN α) where
  parseJSON (Number s) = do
    let r = toRational s
        check t x = if x < 0
                    then fail $ [fmt|negative %t found in ratio %w|] t r
                    else return $ fromIntegral x
    n ← check "numerator"   $ numerator r
    d ← check "denominator" $ denominator r
    return $ MRatioN (n % d)
  parseJSON (String s) =
    case rational s of
      𝓡 (d,_) → pure $ MRatioN d
      𝓛  e    → fail $ [fmt|failed to parse Ratio: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for MRatioN: '%w'|] v

------------------------------------------------------------

newtype FrameRate = FrameRate (MRatioN ℕ)  deriving  (Eq, FromJSON, Num, Show)

------------------------------------------------------------

data TrackGeneral =
  TrackGeneral { _videoCount ∷ MWord Word8
               , _audioCount ∷ MWord Word8
               , _textCount  ∷ MWord Word8
               , _duration   ∷ MDuration
               , _fileSize   ∷ MWord Word64
               , _frameRate  ∷ FrameRate
               }
  deriving (Eq, Show)

instance FromJSON TrackGeneral where
  parseJSON = withObject "TrackGeneral" $ \ v →
    TrackGeneral ⊳ v .: "VideoCount"
                 ⊵ v .: "AudioCount"
                 ⊵ v .: "TextCount"
                 ⊵ v .: "Duration"
                 ⊵ v .: "FileSize"
                 ⊵ v .: "FrameRate"

------------------------------------------------------------

data VideoFormat = VideoFormat_AVC | VideoFormat_HEVC
                 | VideoFormat_MPEG4_Visual
  deriving (Eq,Show)

instance FromJSON VideoFormat where
  parseJSON = withText "VideoFormat" $ \ v →
    case v of
      "AVC"           → pure VideoFormat_AVC
      "HEVC"          → pure VideoFormat_HEVC
      "MPEG-4 Visual" → pure VideoFormat_MPEG4_Visual
      _               → fail $ [fmt|Unknown video format: %t|] v

------------------------------------------------------------

data TrackVideo =
  TrackVideo { _videoFormat ∷ VideoFormat
             , _videoWidth  ∷ MWord Word16
             , _videoHeight ∷ MWord Word16
             }
  deriving (Eq, Show)

instance FromJSON TrackVideo where
  parseJSON = withObject "TrackVideo" $ \ v →
    TrackVideo ⊳ v .: "Format"
               ⊵ v .: "Width"
               ⊵ v .: "Height"

------------------------------------------------------------

data AudioFormat = AudioFormat_AAC | AudioFormat_AC3
                 | AudioFormat_EAC3 | AudioFormat_MPEG
  deriving (Eq,Show)

instance FromJSON AudioFormat where
  parseJSON = withText "AudioFormat" $ \ v →
    case v of
      "AAC"           → pure AudioFormat_AAC
      "AC-3"          → pure AudioFormat_AC3
      "E-AC-3"        → pure AudioFormat_EAC3
      "MPEG Audio"    → pure AudioFormat_MPEG
      _               → fail $ [fmt|Unknown audio format: %t|] v

------------------------------------------------------------

data Language = Language_EN | Language_Other 𝕋 deriving (Eq,Show)

instance FromJSON Language where
  parseJSON = withText "Language" $ \ t →
    pure $ case t of
      "en"            → Language_EN
      _               → Language_Other t

------------------------------------------------------------

data IsDefault = IsDefault | IsntDefault
  deriving (Eq,Show)

instance FromJSON IsDefault where
  parseJSON (Bool b)   = pure $ bool IsntDefault IsDefault b
  parseJSON (String t) = case t of
                           "Yes" → pure IsDefault
                           "No"  → pure IsntDefault
                           _     → fail $ [fmt|Default: bad value '%t'|] t
  parseJSON v          = fail $ [fmt|Default: bad value type '%w'|] v

------------------------------------------------------------

data IsForced = IsForced | IsntForced
  deriving (Eq,Show)

instance FromJSON IsForced where
  parseJSON (Bool b)   = pure $ bool IsntForced IsForced b
  parseJSON (String t) = case t of
                           "Yes" → pure IsForced
                           "No"  → pure IsntForced
                           _     → fail $ [fmt|Forced: bad value '%t'|] t
  parseJSON v          = fail $ [fmt|Forced: bad value type '%w'|] v

------------------------------------------------------------

data TrackAudio =
  TrackAudio { _audioFormat   ∷ AudioFormat
             , _audioLang     ∷ Language
             , _audioDefault  ∷ IsDefault
             , _audioForced   ∷ IsForced
             , _audioChannels ∷ MWord Word8
             }
  deriving (Eq, Show)

instance FromJSON TrackAudio where
  parseJSON = withObject "TrackAudio" $ \ v →
    TrackAudio ⊳ v .: "Format"
               ⊵ v .: "Language"
               ⊵ v .: "Default"
               ⊵ v .: "Forced"
               ⊵ v .: "Channels"

------------------------------------------------------------

data TextFormat = TextFormat_ASS | TextFormat_PGS | TextFormat_UTF8
                | TextFormat_VobSub
  deriving (Eq,Show)

instance FromJSON TextFormat where
  parseJSON = withText "TextFormat" $ \ v →
    case v of
      "ASS"       → pure TextFormat_ASS
      "PGS"       → pure TextFormat_PGS
      "UTF-8"     → pure TextFormat_UTF8
      "VobSub"    → pure TextFormat_VobSub
      _           → fail $ [fmt|Unknown text format: %t|] v

------------------------------------------------------------

data TextTitle = TextTitle_EN_SDH | TextTitle_EN_US | TextTitle_EN_Forced
               | TextTitle_SDH | TextTitle_EN | TextTitle_Other 𝕋
  deriving (Eq,Show)

instance FromJSON TextTitle where
  parseJSON = withText "TextTitle" $ \ t →
    case t of
      "English (SDH)"           → pure TextTitle_EN_SDH
      "ENG NON UDENTI - PGS"    → pure TextTitle_EN_SDH
      "ENG NON UDENTI - SRT"    → pure TextTitle_EN_SDH
      "SDH"                     → pure TextTitle_SDH
      "English (United States)" → pure TextTitle_EN_US
      "ENG REGULAR - PGS"       → pure TextTitle_EN
      "ENG REGULAR - SRT"       → pure TextTitle_EN
      "ENG FORCED - PGS"        → pure TextTitle_EN_Forced
      "ENG FORCED - SRT"        → pure TextTitle_EN_Forced
      _                         → pure $ TextTitle_Other t

------------------------------------------------------------

data TrackText =
  TrackText { _textFormat   ∷ TextFormat
            , _textLang     ∷ Language
            , _textDefault  ∷ IsDefault
            , _textForced   ∷ IsForced
            , _textTitle    ∷ TextTitle
            , _textCodecID  ∷ 𝕋
            }
  deriving (Eq, Show)

instance FromJSON TrackText where
  parseJSON = withObject "TrackText" $ \ v →
    TrackText ⊳ v .: "Format"
              ⊵ v .: "Language"
              ⊵ v .: "Default"
              ⊵ v .: "Forced"
              ⊵ v .: "Title"
              ⊵ v .: "CodecID"

------------------------------------------------------------

data Track = TrackTypeGeneral TrackGeneral
           | TrackTypeVideo   TrackVideo
           | TrackTypeAudio   TrackAudio
           | TrackTypeText    TrackText
  deriving  (Eq,Show)

----------

instance FromJSON Track where
    parseJSON = withObject "Track" $ \ v →
      case v !? "@type" of
        𝓙 (String "General") → TrackTypeGeneral ⊳ parseJSON (Object v)
        𝓙 (String "Video")   → TrackTypeVideo   ⊳ parseJSON (Object v)
        𝓙 (String "Audio")   → TrackTypeAudio   ⊳ parseJSON (Object v)
        𝓙 (String "Text")    → TrackTypeText    ⊳ parseJSON (Object v)
        𝓙 t → error $ [fmt|Track type: %w|] t
        𝓝 → error "no @type"

------------------------------------------------------------

newtype Tracks = Tracks { _ttracks ∷ Vector Track }
  deriving (Eq,Monoid,Semigroup,Show)

----------

instance FromJSON Tracks where
    parseJSON = withArray "Tracks" $ \ v →
      Tracks ⊳ mapM parseJSON v

------------------------------------------------------------

data Media = Media { _ref ∷ AbsFile, _tracks ∷ Tracks }  deriving  (Eq,Show)

----------

instance FromJSON Media where
    parseJSON = withObject "Media" $ \ v →
      Media ⊳ (__parse__ @_ @𝕋 ⊳ v .: "@ref") ⊵ v .: "track"

------------------------------------------------------------

data MediaInfo = MediaInfo { media ∷ Media }  deriving (Eq,Show)

----------

instance FromJSON MediaInfo where
    parseJSON = withObject "MediaInfo" $ \ v →
      MediaInfo ⊳ ({- __parse__ @_ @𝕋 ⊳ -} v .: "media")

----------

instance Printable MediaInfo where print = P.string ∘ show

------------------------------------------------------------

mediainfo ∷  ∀ ε γ δ μ . (MonadIO μ, FileAs γ, MonadLog (Log MockIOClass) μ,
                          HasDoMock δ, MonadReader δ μ,
                          AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                          AsProcExitError ε, AsAesonError ε, Printable ε,
                          MonadError ε μ) =>
             γ → μ MediaInfo

mediainfo fn = do
  (_, json) ← ꙩ (Paths.mediaInfo, ["--Output=JSON", toText fn])
  case eitherDecodeStrict @MediaInfo json of
    𝓡 info → return info
    𝓛 e    → throwAsAesonError e

----------

_test_mediainfo ∷ (MonadIO μ, MonadMask μ) => 𝕄 AbsFile → μ ()
_test_mediainfo fn = do
  let movies_dir = [absdir|/local/martyn/Movies/|]
      mickey_dir = [reldir|Mickey 17  [2025]  (15)  1080p  HEVC/|]
      def_fn = movies_dir ⫻ mickey_dir ⫻ [relfile|Mickey 17.mkv|]
      runNoMock = flip runReaderT NoMock
  e ← logit @UsageParseAesonFPPIOError $ runNoMock $ mediainfo (def_fn ⧐ fn)
  case e of
    𝓛 e' → say $ [fmtT|ERROR: %T|] e'
    𝓡 r → say r

----------------------------------------

tests ∷ TestTree
tests =
  let decode = eitherDecode @MediaInfo
  in  testGroup "MediaInfo" $
        let fn          = [absfile|/local/martyn/TV/Alien: Earth - 01x03.mkv|]
            check t e x = testCase t $ assertRight (assertEqual t e) (decode x)
            tgen'       = TrackGeneral 1 1 23 (msecs 3259.552) 2116638261 24
            tgen        = TrackTypeGeneral tgen'
            tvid        = TrackTypeVideo $ TrackVideo VideoFormat_AVC 1920 1080
            taud        = TrackTypeAudio $ TrackAudio AudioFormat_EAC3
                                                      Language_EN IsDefault
                                                      IsntForced 6
            txt_utf8    = "S_TEXT/UTF8"
            txt1        = TrackTypeText $ TrackText TextFormat_UTF8 Language_EN
                                                    IsDefault IsntForced
                                                    TextTitle_EN_SDH txt_utf8
            txt2        = TrackTypeText $ TrackText TextFormat_UTF8
                                                    (Language_Other "cs")
                                                    IsntDefault IsntForced
                                                    (TextTitle_Other "Čeština")
                                                    txt_utf8
        in  [ let txt = "Unexpected end-of-input, expecting JSON value"
              in  testCase "t0" $
                  assertLeft (\ e → assertBool ("t0: " ◇ e) $ txt `isInfixOf` e)
                             (decode t0)
            , check "t1" (MediaInfo (Media [absfile|/file|] ф)) t1
            , let expect = MediaInfo (Media fn ф) in check "t2" expect t2
            , let expect = MediaInfo (Media fn (Tracks $ fromList [tgen]))
              in  check "t3" expect t3
            , let expect = MediaInfo (Media fn (Tracks $ fromList [tgen, tvid]))
              in  check "t4" expect t4
            , let tracks = fromList [tgen, tvid, taud]
                  expect = MediaInfo (Media fn (Tracks tracks))
              in  check "t5" expect t5
            , let tracks = fromList [tgen, tvid, taud, txt1, txt2]
                  expect = MediaInfo (Media fn (Tracks tracks))
              in  check "t6" expect t6
            ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
