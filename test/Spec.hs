transpose' :: Int -> [[a]] ->[[a]]
transpose' n list
  | (length . head) list < n = []
  | otherwise = concatMap (drop (n-1) . take n) list : transpose' (n+1) list

main :: IO ()
main = do
  let tList = transpose' 1 [[1,2,3,4],[2,3,4,5],[3,4,5,6]]
  putStrLn "The result of My Test"
  print tList
  putStrLn "Test End!"
