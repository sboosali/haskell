-- (+) is associative -> foldl (+) = foldr (+)
-- (-) is not associative -> foldl (+) ≠ foldr (+)
-- foldl :associative :left-identity
-- foldr :associative :right-identity
-- (:) is right-associate

rfold f xs x = foldr f x xs

main = do
 print $ foldl (-) 3 [2,1] == (3 - 2) - 1
 print $ rfold (-) [2,1] 0 == 2 - (1 - 0)
 print $ rfold (:) ['a','b'] [] == 'a' : 'b' : []

reverse = foldl (flip (:)) []

