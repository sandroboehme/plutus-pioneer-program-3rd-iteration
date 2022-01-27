-- fmap :: (a -> b) -> f a -> f b
-- fmap reverse getLine

-- reverse :: [a] -> [a]
-- getLine :: IO String

-- fmap ([String] -> [String]) -> IO String -> IO String
main = do line <- fmap reverse getLine
          putStrLn $ "You said- " ++ line ++ " -backwards!"
          putStrLn $ "Yes, you really said- " ++ line ++ " -backwards!"
