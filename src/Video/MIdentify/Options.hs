module Video.MIdentify.Options
  ( DefaultOptions
  , Mode( ModeDefault, ModeTabs )
  , Options
  , ShowChapters( NoShowChapters, ShowChapters )
  , ShowUnparsedFields( NoShowUnparsedFields, ShowUnparsedFields )
  , inputs
  , mode
  , parseOptions
  , showChapters
  , showUnparsedFields
  )
where

import Base1

-- fpath -------------------------------

import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError )

-- monadio-plus ------------------------

import MonadIO.FPath  ( pResolve )

-- optparse-applicative ----------------

import Options.Applicative.Builder ( argument, flag, flag', help, long, metavar,
                                     str )
import Options.Applicative.Types   ( Parser )

-- optparse-plus -----------------------

import OptParsePlus ( parseNE )

--------------------------------------------------------------------------------

data ShowChapters = ShowChapters | NoShowChapters

--------------------

data ShowUnparsedFields = ShowUnparsedFields | NoShowUnparsedFields
  deriving ( Eq, Show )

--------------------

data DefaultOptions = DefaultOptions { _showChapters       :: ShowChapters
                                     , _showUnparsedFields :: ShowUnparsedFields
                                     }

----------------------------------------

showChapters ∷ Lens' DefaultOptions ShowChapters
showChapters = lens _showChapters (\ o sc → o { _showChapters = sc })

--------------------

showUnparsedFields ∷ Lens' DefaultOptions ShowUnparsedFields
showUnparsedFields = lens _showUnparsedFields
                           (\ o uf → o { _showUnparsedFields = uf })

------------------------------------------------------------

data Mode = ModeDefault DefaultOptions
          | ModeTabs

--------------------

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

parseDefaultOptions ∷ Parser DefaultOptions
parseDefaultOptions =
  DefaultOptions ⊳ flag NoShowChapters ShowChapters
                        (long "show-chapters" ⊕ help "show chapter info")
                 ⊵ flag NoShowUnparsedFields ShowUnparsedFields
                        (long "show-unparsed" ⊕ help "show unparsed fields")
----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ (ModeDefault ⊳ parseDefaultOptions ∤
             flag' ModeTabs (long "tabs" ⊕ help "output tab-delimited"))
          ⊵ parseNE (argument str (metavar "FILENAME"))

-- that's all, folks! ----------------------------------------------------------
