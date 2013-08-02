{-# OPTIONS -Wall -Werror #-}

import Data.List

data FizzBuzz = Fizz | Buzz | FizzBuzz | None deriving ( Read , Eq )

main :: IO ( )
main = getContents >>= ( return . map read . parseFizzBuzz ) >>= ( return . solve ) >>= mapM_ print

parseFizzBuzz :: String -> [ String ]
parseFizzBuzz = filter ( ` elem ` [ "Fizz" , "Buzz" , "FizzBuzz" ] ) . words

solve :: [ FizzBuzz ] -> [ Int ]
solve xs = safe_head $ filter isOK $ solveHelper xs

solveHelper :: [ FizzBuzz ] -> [ [ ( FizzBuzz , ( Int , FizzBuzz ) ) ] ]
solveHelper [ ] = [ ]
solveHelper xs @ ( Fizz : _ ) = map ( zip xs ) [ genFizzBuzz 3 , genFizzBuzz 6 , genFizzBuzz 9 , genFizzBuzz 12 ]
solveHelper xs @ ( Buzz : _ ) = map ( zip xs ) [ genFizzBuzz 5 , genFizzBuzz 10 ]
solveHelper xs @ ( FizzBuzz : _ ) = [ zip xs $ genFizzBuzz 15 ]
solveHelper _ = [ ]

safe_head :: [ [ ( FizzBuzz , ( Int , FizzBuzz ) ) ] ] -> [ Int ]
safe_head [ ] = [ ]
safe_head xs = head $ sortBy lengthOrd $ map ( \ xs' -> [ head xs' .. last xs' ] ) $ map ( map ( \ ( _ , ( x , _ ) ) -> x ) ) xs

lengthOrd :: ( Ord a ) => [ a ] -> [ a ] -> Ordering
lengthOrd xs ys
	| length xs /= length ys = compare ( length xs ) ( length ys )
	| otherwise = compare xs ys

genFizzBuzz :: Int -> [ ( Int , FizzBuzz ) ]
genFizzBuzz x
	| x ` mod ` 15 == 0 = ( x , FizzBuzz ) : genFizzBuzz ( x + 1 )
	| x ` mod ` 3 == 0 = ( x , Fizz ) : genFizzBuzz ( x + 1 )
	| x ` mod ` 5 == 0 = ( x , Buzz ) : genFizzBuzz ( x + 1 )
	| otherwise = genFizzBuzz ( x + 1 )

isOK :: [ ( FizzBuzz , ( Int , FizzBuzz ) ) ] -> Bool
isOK = all ( \ ( x1 , ( _ , x2 ) ) -> x1 == x2 )