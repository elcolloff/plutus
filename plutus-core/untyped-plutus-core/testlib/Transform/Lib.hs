{-# LANGUAGE TypeApplications #-}

{-| Common UPLC term-construction helpers shared across the
@Transform.*.Spec@ test modules. -}
module Transform.Lib
  ( T
  , var
  , lam
  , app
  , force
  , delay
  , case_
  , builtin
  , constr
  , true
  , false
  , con
  , text
  , err
  , name
  , freshNames2
  , freshNames3
  , freshNames4
  , freshNames5
  , freshNames8
  ) where

import Data.Text (Text)
import Data.Word (Word64)
import GHC.Exts (fromList)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.MkPlc (mkConstant)
import PlutusCore.Name.Unique (Name)
import PlutusCore.Quote (freshName, runQuote)
import UntypedPlutusCore.Core.Type (Term (..))

-- | Convenient alias used throughout the test modules.
type T = Term Name DefaultUni DefaultFun ()

var :: Name -> T
var = Var ()

lam :: Name -> T -> T
lam = LamAbs ()

app :: T -> T -> T
app = Apply ()

force :: T -> T
force = Force ()

delay :: T -> T
delay = Delay ()

case_ :: T -> [T] -> T
case_ scrut branches = Case () scrut (fromList branches)

builtin :: DefaultFun -> T
builtin = Builtin ()

-- | A 'Constr' term tagged with the given index.
constr :: Word64 -> [T] -> T
constr = Constr ()

-- | The standard UPLC encoding of @True@ as @Constr 0 []@.
true :: T
true = constr 0 []

-- | The standard UPLC encoding of @False@ as @Constr 1 []@.
false :: T
false = constr 1 []

-- | An 'Integer' constant.
con :: Integer -> T
con = mkConstant @Integer ()

-- | A 'Text' constant.
text :: Text -> T
text = mkConstant @Text ()

err :: T
err = Error ()

-- | Generate a fresh 'Name' from a textual hint.
name :: Text -> Name
name = runQuote . freshName

-- | Generate two fresh 'Name's with sequential 'Unique's.
freshNames2 :: Text -> Text -> (Name, Name)
freshNames2 a b = runQuote $ (,) <$> freshName a <*> freshName b

-- | Generate three fresh 'Name's with sequential 'Unique's.
freshNames3 :: Text -> Text -> Text -> (Name, Name, Name)
freshNames3 a b c = runQuote $ (,,) <$> freshName a <*> freshName b <*> freshName c

-- | Generate four fresh 'Name's with sequential 'Unique's.
freshNames4 :: Text -> Text -> Text -> Text -> (Name, Name, Name, Name)
freshNames4 a b c d =
  runQuote $ (,,,) <$> freshName a <*> freshName b <*> freshName c <*> freshName d

-- | Generate five fresh 'Name's with sequential 'Unique's.
freshNames5 :: Text -> Text -> Text -> Text -> Text -> (Name, Name, Name, Name, Name)
freshNames5 a b c d e =
  runQuote $
    (,,,,) <$> freshName a <*> freshName b <*> freshName c <*> freshName d <*> freshName e

-- | Generate eight fresh 'Name's with sequential 'Unique's.
freshNames8
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> (Name, Name, Name, Name, Name, Name, Name, Name)
freshNames8 a b c d e f g h =
  runQuote $
    (,,,,,,,)
      <$> freshName a
      <*> freshName b
      <*> freshName c
      <*> freshName d
      <*> freshName e
      <*> freshName f
      <*> freshName g
      <*> freshName h
