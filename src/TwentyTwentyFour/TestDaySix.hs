{-# LANGUAGE QuasiQuotes #-}

module TwentyTwentyFour.TestDaySix (tests06) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ
import TwentyTwentyFour.DaySix (numberOfObstructions, numberOfPositions)

tests06 :: TestTree
tests06 =
  testGroup
    "Tests for 06"
    [ testCase "numberOfPositions" $ numberOfPositions zeMap @?= 4656
    -- , testCase "numberOfObstructions" $ numberOfObstructions zeMap @?= 55
    ]

zeMap :: String
zeMap =
  [r|..................#................................................................#........#.....................................
...#...........#...................................................#........................................#.................#...
...................................#................#.#...............#.................................................#.........
.....#......#................#.....................................................................#..........#........#......#...
............................................................................................#.....................#...............
.....................#..##...#........................#.................#.......................#..#.........#.......#...#......#.
............##....................................##..................#...............................#....#......................
.............#............#.#..#.......#...........#..............#...............#.....#.........................................
....................#..........##.........#.........#................#............................................................
....#.......#................................................................#...#.#..........................................#.#.
..#..............#.....................................#..........#.#....#..............#..................#.#............#.......
.#.......#.........................#........................#..#.....#............................................................
........#...#......................#...........#......................................#....................#......................
.............................#..............#........##....#....................#......#....................#............#......#.
..............#..............#............#.......................##....#..........................................#.........#....
.............#............#..........#..........#...#.....................................................#.......................
..#........#.....#..........................................#................................................#...#.#...#..........
........#.....................#...#..#......#.........................................................#.....##.........#..........
..............................................................................................#...#...............................
..#.......................#..........................#......#...................................................................#.
.......#.........................#..............#.........#.............#.......................................#...#.............
..................#.................#.......................#....................#....................................#...........
...#...#.......#......##.#...............#....#..............#..........................................#....#..#.................
#................................#.......#.....#.......#.............#..................#........#................................
...........#..................................#.......................................................................#.#..#......
.......#........#.........................................#..........................#...........................#.....#..........
................#..#.....................#..#...........................#...........................................#.............
#...#.......................#................................................#................#................................#..
..........#...........................#.......#............................................................................#.....#
..................................#............................................................#..................................
......................#........#.............#....#.....#.......#..........................#..........................#...........
...........................##.................#.............#........................#...#..................#.....................
.............................................................................................................#....#..........#....
................#...................................#...........#..........#....................#............................#....
........#....#................................................#................##..#..................................#...........
...........#....................................................#...#.#......#....................................................
.......................#.........................................................^....................................#...........
..........................#.............##..#........#.#....#.......#....................#...........#.........#.....#............
........#...........................#..#..........................................................#...............................
....#....................................#....#.........................................#.........#...............................
...............#...................................#.....#.................................#....................................#.
......#...............................................................#.............................#........................#....
.........................#.....#.........................#.#...............#.........#.....#..................#..#.........#......
.........................................................#......#........##.......................#...#......#................#...
.......................................#.....#.................................#..................................................
..................#..................................#..........................................................................#.
...................#....#.........#.......#................#....................#.........................#......#.....#..#.......
................................#..............................................................................#....#.............
.....................#.#.............#.................#..........#.......#.........................................#.............
......#....................#....................................................................#...#.............................
........#........#...........#.##.............#........................#............#..............#.........#....#........#......
........................#.................................................................#.......................................
............................................................#.....#....................#............#.....#....#............#.....
...........#............#....#........................................................##............#..............#..............
...........#...........#.....#..............#..............##....#........#......#................................................
..#................................................................................#......................#......#................
....................#.........#......................#.............................#.......#.......................#.............#
..#...#....#......................................................................................................#...............
...........#................................................................................#...............................#.....
....................................#............#..........#.........................#..................................#.......#
...................#......#...................#............#......#........#................#.............#...........#.........#.
........................#..................................#........#..................#..........................................
............#..#...........#..................##....................#...........................................#.................
.......................................................................................#.....#............#.......................
...............................................#....#................................#..#..........................#..#...........
..............#..#.....#.........#....................#.......#..#..............#...............................#.....#...........
..........#............................#..........................................................................................
........................................#........#........................................................#.#.....................
#................#......................#..................................................................................#......
...............................................................................#................................#.................
...................................................................................................................#.#........#...
#........................................................................#......................................#.#...............
...#......#............#...................#.............#........#..........................................#......#.............
..............##....#...................................................#......................#..................................
...............#........................................................................#...........#...#.#.......................
..........#.#.....#................................#...........#........#.......#.................................................
#....................#......................................................................................#.....................
..............#...##...........................................#.............#.......#..................#..............#..........
.#.............................................#..............................................................................#...
...#................................#............................................#............................#...................
..............................#......................................................#..........................................#.
...................#.....................................#..................................#......#..............................
....#.#..........##.............#.......#........................##..........#.................#..................................
#..#................#........##....................................................................................#..............
........#...#.........................................................................#...........................................
......................................................#....#...................#..#.................................#.............
........#...............#........#..............................................#............................#....................
...#..............................#.........................................##.......................................#....#.......
...#................#..............#.......#......................................................................................
........#..................#.............................................................#...........................#....#.......
......................#.#..........#.......#..................................#..#...#............#...............#...............
.....#..................#....#..#......#.......................#.....................................................#............
.....#................................#.........................#..................#.#..........#...#............#.....#..........
....#...................#....#.#.........#........#..................................................................#....#..#....
........#...............#......................................#........#..............................#..........................
....#...#.#.....#..............#..................#..........#...#..#......................#...#..........#......................#
...........#...................................................................................................#................#.
.................................##..............#.............................#...............................#..................
...........#.................#.............##...................................#.......................#.................##......
.....#..................#......#........#.................................................#..#........................#..#........
......#.......#......................................................................#.#.........#....#...........................
..................#.#........................#......#................................................................#......#.....
.....................#............#...................#.....#....................................#.......#..........#.......#.....
#.............................#...............................#...#...............................................................
..##.........#........#......................................#.....................#...#.............#............#..#............
..........#.....................#..#....................#...#....................#....................#.....................#.....
...........#..........#...............#..#..........#..............................................#.#............................
............#...#...#.............#.........#.............................#...............#.......................................
.........#...........#...............................................................#............................................
.#..#.................................................................#............#.#.......................##...................
........##............#...................#................................#......................................................
............................................#..........................#..........................................................
#...................#...........#..............#...#............#.#........#.................#......#.............................
........#....................................#....................................................................................
...........#.........#..#...............#...........#.....##.#......#....##............#....#....#.#.........##.........#.........
..#......................#..........#.................#.................#......#.............................#........#...........
....................#.................#....#..............................#....#.............#....#....#.....................#....
............#.................#.................................#...............................#..#...........................#..
...................................................#.....#..............#.......#....................#.#..........................
..#.........##..#........#...................................................................................#............#.......
.....#..............................#......................................................................#........#.............
................................#........##.......#...............#..............#.......#...#.#........#.........................
..............................#.....................#.........................................#..................................#
...........#...................##.................#.................................#...............................#........#....
..............................#...##......#..................#...#................................#......#.......#...............#
.....................##......#......#.#.......#..............#......#.................#.........#...................#.............
..........................#................................#...........#..........................................................
...#.....#....................#.....................#...#.....#.............................#.#....#.....#.#.................#....
............................#...#........#......................................................................#.....#...........
............#..........##..................#.............................................................#.....#..#...............|]
