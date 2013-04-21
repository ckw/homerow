import Text.ParserCombinators.Parsec
import System.Environment
import Control.Applicative ((<$>))
import Control.Monad (when, void, (=<<))

homerowFile = do code <- concat <$> many1 line
                 eof
                 return code

line = do code <- sepBy (many (oneOf "asdfjkl;")) (many1 $ oneOf " \t")
          optional comment
          char '\n'
          return code

comment = char '#' >> many (noneOf "\n")

parseHR input = parse homerowFile "so much fail" input

isBalanced input = case parse balance "jumps unbalanced" input of
                       Left _ -> False
                       Right _ -> True

balance' = do
    char 'a'
    nonJumps
    optional balance'
    nonJumps
    char ';'

balance = nonJumps >> optional (many balance') >> nonJumps >> eof

nonJumps = many (oneOf "sdfjkl")

main = do args <- getArgs
          if elem "-t" args
          then putStrLn $ "Tests pass: " ++ show runAllTests
          else do
              input <- readFile =<< head <$> getArgs
              let raw = case parseHR input of
                               Left e -> show e
                               Right l -> concat l
              when (isBalanced raw) $ putStrLn raw

data Op = IncrementDataPointer
        | DecrementDataPointer
        | IncrementByte
        | DecrementByte
        | InputByte
        | OutputByte
        | JumpForward
        | JumpBack

test1 = isBalanced "a;" == True
test2 = isBalanced "a;a;" == True
test3 = isBalanced "aaa;;;" == True
test4 = isBalanced "aa;" == False
test5 = isBalanced "asdf;sdf" == True
test6 = isBalanced "asdf;" == True
test7 = isBalanced "sdf;" == False
test8 = isBalanced ";a" == False
test9 = isBalanced "a;a;aa;;" == True

tests = [
          test1
        , test2
        , test3
        , test4
        , test5
        , test6
        , test7
        , test8
        , test9
        ]

runAllTests = foldr (&&) True tests
