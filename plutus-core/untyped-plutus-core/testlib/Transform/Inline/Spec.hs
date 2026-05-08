{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transform.Inline.Spec where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import PlutusCore.Annotation (Inline (MayInline))
import PlutusCore.Default (DefaultFun (..), DefaultUni)
import PlutusCore.Name.Unique (Name)
import PlutusCore.Quote (runQuote)
import PlutusPrelude (def)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Transform.Lib (T, app, builtin, case_, delay, freshNames3, freshNames4, lam, var)
import UntypedPlutusCore.AstSize (AstSize (..))
import UntypedPlutusCore.Core (Term (..))
import UntypedPlutusCore.Transform.Inline
  ( Ann
  , InlineHints (..)
  , InlineInfo (..)
  , InlineM
  , S (..)
  , Subst (..)
  , TermEnv (..)
  , effectSafe
  , isFirstVarBeforeEffects
  , isStrictIn
  )

test_inline :: TestTree
test_inline =
  testGroup
    "Inline"
    [ testCase
        "var is before or after effects"
        testVarBeforeAfterEffects
    , testGroup
        "isStrictIn"
        [ testCase
            "a var is delayed if it's inside a delay"
            testVarIsEventuallyEvaluatedDelay
        , testCase
            "a var is delayed if it's inside a lambda"
            testVarIsEventuallyEvaluatedLambda
        , testCase
            "a var is delayed if it's inside a case branch"
            testVarIsEventuallyEvaluatedCaseBranch
        ]
    , testGroup
        "effectSafe"
        [ testCase "with preserved logs" testEffectSafePreservedLogs
        , testCase "without preserved logs" testEffectSafeWithoutPreservedLogs
        ]
    ]

testVarBeforeAfterEffects :: Assertion
testVarBeforeAfterEffects = do
  assertBool "a is evaluated before effects" do
    isFirstVarBeforeEffects def a term
  assertBool "b is evaluated before effects" do
    isFirstVarBeforeEffects def b term
  assertBool "c is not evaluated before effects" $ not do
    isFirstVarBeforeEffects def c term
  where
    term :: Term Name DefaultUni DefaultFun ()
    term =
      {- Evaluation order:

        1. pure work-free: a
        2. pure work-free: b
        3. impure? maybe work?: plus a b
        4. pure work-free: c
        5. impure? maybe work?: plus (plus a b) c
      -}
      plus (plus (var a) (var b)) (var c)
    (a, b, c) = freshNames3 "a" "b" "c"

testVarIsEventuallyEvaluatedDelay :: Assertion
testVarIsEventuallyEvaluatedDelay = do
  assertBool "var 'a' is not eventually evaluated in delay" $
    not (isStrictIn a term)
  assertBool "var 'b' is eventually evaluated outside of the delay" $
    isStrictIn b term
  assertBool "it's not known if var 'c' is eventually evaluated" $
    not (isStrictIn c term)
  where
    term :: Term Name DefaultUni DefaultFun ()
    term = delay (var a `plus` var b) `plus` var b

    (a, b, c) = freshNames3 "a" "b" "c"

testVarIsEventuallyEvaluatedLambda :: Assertion
testVarIsEventuallyEvaluatedLambda = do
  assertBool "var 'a' is not eventually evaluated in lambda body" $
    not (isStrictIn a term)
  assertBool "var 'c' is eventually evaluated outside of the lambda" $
    isStrictIn c term
  assertBool "it's not known if var 'd' is eventually evaluated" $
    not (isStrictIn d term)
  where
    term :: Term Name DefaultUni DefaultFun ()
    term = lam b (var a `plus` var c) `app` var c

    (a, b, c, d) = freshNames4 "a" "b" "c" "d"

testVarIsEventuallyEvaluatedCaseBranch :: Assertion
testVarIsEventuallyEvaluatedCaseBranch = do
  assertBool "var 'a' is not eventually evaluated in case branch" $
    not (isStrictIn a term)
  assertBool "var 'b' is eventually evaluated outside of the case branch" $
    isStrictIn b term
  assertBool "it is not known if var 'd' is eventually evaluated" $
    not (isStrictIn d term)
  where
    term :: Term Name DefaultUni DefaultFun ()
    term = case_ (var b) [var a, var b, var c]

    (a, b, c, d) = freshNames4 "a" "b" "c" "d"

testEffectSafePreservedLogs :: Assertion
testEffectSafePreservedLogs = do
  assertBool "an immediately eval'd var is not \"effect safe\"" $
    runInlineWithLogging (not <$> effectSafe term c False)
  assertBool "a var before effects is \"effect safe\"" $
    runInlineWithLogging (effectSafe term a False)
  where
    term :: Term Name DefaultUni DefaultFun ()
    term = (var a `plus` var b) `plus` var c

    (a, b, c) = freshNames3 "a" "b" "c"

testEffectSafeWithoutPreservedLogs :: Assertion
testEffectSafeWithoutPreservedLogs = do
  assertBool "an immediately eval'd var is \"effect safe\"" $
    runInlineWithoutLogging (effectSafe term c False)
  assertBool "a var before effects is \"effect safe\"" $
    runInlineWithoutLogging (effectSafe term a False)
  where
    term :: Term Name DefaultUni DefaultFun ()
    term = (var a `plus` var b) `plus` var c

    (a, b, c) = freshNames3 "a" "b" "c"

--------------------------------------------------------------------------------
-- InlineM runner --------------------------------------------------------------

runInlineWithoutLogging :: InlineM Name DefaultUni DefaultFun () r -> r
runInlineWithoutLogging = runInlineM False

runInlineWithLogging :: InlineM Name DefaultUni DefaultFun () r -> r
runInlineWithLogging = runInlineM True

runInlineM :: Bool -> InlineM Name DefaultUni DefaultFun () r -> r
runInlineM preserveLogging m = result
  where
    (result, _finalState) =
      runQuote (runStateT (runReaderT m inlineInfo) initialState)
    inlineInfo :: InlineInfo Name DefaultFun ()
    inlineInfo =
      InlineInfo
        { _iiUsages = mempty
        , _iiHints = InlineHints \_ann _name -> MayInline
        , _iiBuiltinSemanticsVariant = def
        , _iiInlineConstants = True
        , _iiInlineUnconditionalGrowth = AstSize 3
        , _iiInlineCallsiteGrowth = AstSize 1_000_000
        , _iiPreserveLogging = preserveLogging
        }
    initialState :: S Name DefaultUni DefaultFun (Ann ())
    initialState = S {_subst = Subst (TermEnv mempty), _vars = mempty}

--------------------------------------------------------------------------------
-- Local helpers ---------------------------------------------------------------

plus :: T -> T -> T
plus i j = builtin AddInteger `app` i `app` j
