module Test.HUnit.Unicode
(
	module Test.HUnit,
	(@=?),
	(@?=),
	(~=?),
	(~?=),
	assertEqual
) where

import Test.HUnit hiding ((@=?), (@?=), (~=?), (~?=), assertEqual)
import qualified Test.HUnit as HUnit

import GHC.Stack (HasCallStack)
import Text.Show.Unicode (ushow)

newtype UWrap a = UWrap a deriving (Eq)
instance Show a => Show (UWrap a) where
	show (UWrap a) = ushow a

(@=?) :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
expected @=? actual = UWrap expected HUnit.@=? UWrap actual

(@?=) :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
actual @?= expected = UWrap actual HUnit.@?= UWrap expected

(~=?) :: (HasCallStack, Eq a, Show a) => a -> a -> Test
expected ~=? actual = TestCase (expected @=? actual)

(~?=) :: (HasCallStack, Eq a, Show a) => a -> a -> Test
actual ~?= expected = TestCase (actual @?= expected)

assertEqual :: (HasCallStack, Eq a, Show a) => String -> a -> a -> Assertion
assertEqual preface expected actual = HUnit.assertEqual preface (UWrap expected) (UWrap actual)