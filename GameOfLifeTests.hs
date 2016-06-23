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

tickTests = TestList [TestLabel "blockPattern" block, TestLabel "boatPattern" boat, TestLabel "blinkerPattern" blinker, TestLabel "toadPattern" toad]

block1 = TestCase (assertBool  ("test1 failed - result:" ++ (show $ tick [(1, 1), (1, 2), (2, 1), (2, 2)]))
		(ticks [(1, 1), (1, 2), (2, 1), (2, 2)] 1 == [(1, 1), (1, 2), (2, 1), (2, 2)]))

boat1 = TestCase (assertBool  ("test2 failed - result:" ++ (show $ tick [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)]))
		(ticks [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)] 1 == [(0, 1),  (0, 2), (1, 0), (1, 2), (2, 1)]))

blinker1 = TestCase (assertBool  ("test3 failed - result:" ++ (show $ tick [(1,1), (1, 0), (1, 2)]))
		(ticks [(1,1), (1, 0), (1, 2)] 1 == [(0,1), (1, 1), (2, 1)]))

toad1 = TestCase (assertBool ("test4 failed - result:" ++ (show $ tick [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)]))
		(ticks [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)] 1 == [(0, 2),(1, 1),(1, 4),(2, 1),(2, 4),(3, 3)]))

block2 = TestCase (assertBool  ("test1 failed - result:" ++ (show $ tick [(1, 1), (1, 2), (2, 1), (2, 2)]))
		(ticks [(1, 1), (1, 2), (2, 1), (2, 2)] 3 == [(1, 1), (1, 2), (2, 1), (2, 2)]))

		
		
ticksTests = TestList [TestLabel "blockPattern" block1, TestLabel "blockPattern - 3 gens" block2, 
						TestLabel "boatPattern" boat1, 
						TestLabel "blinkerPattern" blinker1, TestLabel "toadPattern" toad1]