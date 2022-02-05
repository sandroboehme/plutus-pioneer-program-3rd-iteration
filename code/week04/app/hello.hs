-- public static int foo() {
-- ...
-- }
--
-- ...
--
-- ... foo() ... foo()

-- foo :: Int
-- foo = ...

-- let x = foo in ... x ... x ...
-- ... foo ... foo ...

-- foo :: IO Int
-- foo = ...

main :: IO ()
main = bar

bar :: IO ()
bar = getLine >>= \s ->
     getLine >>= \t ->
     putStrLn (s ++ t)
