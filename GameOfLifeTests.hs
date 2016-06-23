import GameOfLife
import Test.QuickCheck
import Test.HUnit

block = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(1, 1), (1, 2), (2, 1), (2, 2)]))
		(tick [(1, 1), (1, 2), (2, 1), (2, 2)] == [(1, 1), (1, 2), (2, 1), (2, 2)]))

boat = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)]))
		(tick [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)] == [(0, 1),  (0, 2), (1, 0), (1, 2), (2, 1)]))

blinker = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(1,1), (1, 0), (1, 2)]))
		(tick [(1,1), (1, 0), (1, 2)] == [(0,1), (1, 1), (2, 1)]))

toad = TestCase (assertBool ("failed - result:" ++ (show $ tick [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)]))
		(tick [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)] == [(0, 2),(1, 1),(1, 4),(2, 1),(2, 4),(3, 3)]))

tickTests = TestList [TestLabel "blockPattern" block, TestLabel "boatPattern" boat, TestLabel "blinkerPattern" blinker, TestLabel "toadPattern" toad]

block1 = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(1, 1), (1, 2), (2, 1), (2, 2)]))
		(ticks 1 [(1, 1), (1, 2), (2, 1), (2, 2)] == [(1, 1), (1, 2), (2, 1), (2, 2)]))

boat1 = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)]))
		(ticks 1 [(0, 1), (1, 0), (2, 1), (0, 2), (1, 2)] == [(0, 1),  (0, 2), (1, 0), (1, 2), (2, 1)]))

blinker1 = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(1,1), (1, 0), (1, 2)]))
		(ticks 1 [(1,1), (1, 0), (1, 2)] == [(0,1), (1, 1), (2, 1)]))

toad1 = TestCase (assertBool ("failed - result:" ++ (show $ tick [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)]))
		(ticks 1 [(1, 1),(1, 2),(1, 3),(2, 2),(2, 3),(2, 4)] == [(0, 2),(1, 1),(1, 4),(2, 1),(2, 4),(3, 3)]))

block2 = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(1, 1), (1, 2), (2, 1), (2, 2)]))
		(ticks 3 [(1, 1), (1, 2), (2, 1), (2, 2)] == [(1, 1), (1, 2), (2, 1), (2, 2)]))

blinker2 = TestCase (assertBool  ("failed - result:" ++ (show $ tick [(1,1), (1, 0), (1, 2)]))
		(ticks 2 [(1,1), (1, 0), (1, 2)] == [(1,0), (1, 1), (1, 2)]))
		
		
ticksTests = TestList [TestLabel "blockPattern" block1, TestLabel "blockPattern - 3 gens" block2, 
						TestLabel "boatPattern" boat1, TestLabel "blinkerPattern" blinker1, 
						TestLabel "blinkerPattern - 2 gens" blinker2, TestLabel "toadPattern" toad1]