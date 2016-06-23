import GameOfLife
import Test.QuickCheck
import Test.HUnit

block = TestCase (assertBool  ("test1 failed - result:" ++ (show $ tick [(1, 1), (1, 2), (2, 1), (2, 2)]))
		(tick [(1, 1), (1, 2), (2, 1), (2, 2)] == [(1, 1), (1, 2), (2, 1), (2, 2)]))

boat = TestCase (assertBool  ("test2 failed - result:" ++ (show $ tick [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)]))
		(tick [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)] == [(0, 1),  (0, 2), (1, 0), (1, 2), (2, 1)]))

blinker = TestCase (assertBool  ("test3 failed - result:" ++ (show $ tick [(1,1), (1, 0), (1, 2)]))
		(tick [(1,1), (1, 0), (1, 2)] == [(0,1), (1, 1), (2, 1)]))

toad = TestCase (assertBool ("test4 failed - result:" ++ (show $ tick [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)]))
		(tick [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)] == [(0, 2),(1, 1),(1, 4),(2, 1),(2, 4),(3, 3)]))

tests = TestList [TestLabel "blockPattern" block, TestLabel "boatPattern" boat, TestLabel "blinkerPattern" blinker, TestLabel "toadPattern" toad]