-- Requires ghc and quickcheck.
--
-- > brew install ghc cabal-install
-- > cabal update
-- > cabal install quickcheck
import Test.QuickCheck

--Test Data Generation---------------------------------------------------------
singleDigit :: Gen Integer
singleDigit = suchThat arbitrary (\i -> 0 <= i && i <= 9)

validDigitStream :: Gen [Integer]
-- Stream should not start with 0,
-- Stream size should be beyond Int range
-- length(show(maxBound :: Int)) == 19
validDigitStream = suchThat (listOf1 singleDigit) (\ds -> head ds /= 0 && length ds > 20)

--Test-------------------------------------------------------------------------
prop_add_one_result_should_match_naive_implementation :: Property
prop_add_one_result_should_match_naive_implementation =
    forAll (validDigitStream) (\ds -> naiveImp ds == addOne ds)

prop_split_is_inverse_of_join :: Property
prop_split_is_inverse_of_join = forAll (validDigitStream) (\ds -> (splitDigits (joinDigits ds) ) == ds)

--Solution For addOne----------------------------------------------------------

-- Cannot pattern match from the tail, so let's process the list from tail
-- Then reverse the result
addOne :: [Integer] -> [Integer]
addOne ds = reverse( addOneHelper 1 (reverse(ds)) )

-- Remeber, input is reversed
-- So you are given [1,2,3] and should return [2,2,3]
addOneHelper :: Integer -> [Integer] -> [Integer]
addOneHelper 0 xs = xs
addOneHelper 1 [] = [1]
addOneHelper 1 (9:xs) = [0] ++ addOneHelper 1 xs
addOneHelper 1 (x:xs) = [(x+1)] ++ xs

--Reference Naive solution-----------------------------------------------------
naiveImp :: [Integer] -> [Integer]
naiveImp ds = splitDigits $ 1 + joinDigits ds

splitDigits n = map (\d -> (read [d] :: Integer)) (show n)
joinDigits ds = foldl (\acc d -> 10*acc + d) 0 ds

--Main-------------------------------------------------------------------------
main = do
    putStrLn "Checking split is inverse of join"
    quickCheck prop_split_is_inverse_of_join
    putStrLn "Checking add_one against naive implementation"
    quickCheck prop_add_one_result_should_match_naive_implementation
