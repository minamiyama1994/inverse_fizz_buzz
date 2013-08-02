{-# OPTIONS -Wall #-}

data FizzBuzz = Fizz | Buzz | FizzBuzz | None deriving ( Read , Eq )

main :: IO ( )
main = getContents >>= ( return . map read . parseFizzBuzz ) >>= ( return . solve ) >>= ( return . showAnswer ) >>= putStr

parseFizzBuzz :: String -> [ String ]
parseFizzBuzz = filter ( ` elem ` [ "Fizz" , "Buzz" , "FizzBuzz" ] ) . lines

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
safe_head xs = case map ( \ ( _ , ( x , _ ) ) -> x ) $ head xs of
	xs' -> [ head xs' .. last xs' ]

genFizzBuzz :: Int -> [ ( Int , FizzBuzz ) ]
genFizzBuzz x
	| x ` mod ` 15 == 0 = ( x , FizzBuzz ) : genFizzBuzz ( x + 1 )
	| x ` mod ` 3 == 0 = ( x , Fizz ) : genFizzBuzz ( x + 1 )
	| x ` mod ` 5 == 0 = ( x , Buzz ) : genFizzBuzz ( x + 1 )
	| otherwise = genFizzBuzz ( x + 1 )

isOK :: [ ( FizzBuzz , ( Int , FizzBuzz ) ) ] -> Bool
isOK = all ( \ ( x1 , ( _ , x2 ) ) -> x1 == x2 )

showAnswer :: [ Int ] -> String
showAnswer xs = concat $ map ( ++ "\n" ) $ map show xs