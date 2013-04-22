{-# LANGUAGE RecordWildCards #-}

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Debug.Trace
import Control.Applicative ((<$>))
import Control.Monad (unless, when, void, (=<<))
import Data.IORef
import Data.Maybe
import Data.Sequence ((><))
import qualified Data.Sequence as S
import Data.Sequence
import Data.Word

main = do args <- getArgs
          if elem "-t" args
          then putStrLn $ "Tests pass: " ++ show runAllTests
          else do
              input <- readFile =<< head <$> getArgs
              let raw = case parseHR input of
                               Left e -> show e
                               Right l -> concat l
              unless (isBalanced raw) $ error "unbalanced input"
              --genST raw >>= printST
              node <- genST raw
              let state = S.replicate 30000 1
                  pointer = 0
                  loop programState = do
                      res <- step programState
                      case res of
                          Nothing -> putStrLn ""
                          Just ps -> loop ps
              loop $ ProgramState state (Just node) pointer

data ProgramState = ProgramState (S.Seq Word8) (Maybe Node) Pointer

type Pointer = Int

step :: ProgramState -> IO (Maybe ProgramState)
step ps@(ProgramState st mb_node pointer) = do
    case mb_node of
        Nothing -> return Nothing
        Just node ->
            case nOp node of
                IncrementDataPointer -> do
                    let pointer' = pointer + 1
                    next <- readIORef $ nNext node
                    if (pointer' == S.length st)
                    then return $
                             Just $
                             ProgramState (st >< (S.replicate pointer' 0 ))
                                 next (pointer')
                    else return $ Just $ ProgramState st next pointer'

                DecrementDataPointer -> do
                    let pointer' = pointer - 1
                    next <- readIORef $ nNext node
                    if (pointer' < 0)
                    then return $
                             Just $
                             ProgramState ((S.replicate (S.length st) 0 ) >< st)
                                 next (S.length st)
                    else return $ Just $ ProgramState st next pointer'

                IncrementByte -> do let st' = adjust (+1) pointer st
                                    next <- readIORef $ nNext node
                                    return $ Just $ ProgramState st' next pointer

                DecrementByte -> do let st' = adjust (\w -> w - 1) pointer st
                                    next <- readIORef $ nNext node
                                    return $ Just $ ProgramState st' next pointer
                InputByte -> do
                    next <- readIORef $ nNext node
                    return $ Just $ ProgramState st next pointer

                OutputByte -> do
                    next <- readIORef $ nNext node
                    hPutChar stdout (toEnum $ fromIntegral $ S.index st pointer)
                    hFlush stdout
                    return $ Just $ ProgramState st next pointer

                JumpForward -> do nextJump <- readIORef $ nNextJump node
                                  return $ Just $ ProgramState st nextJump pointer
                JumpBack -> do prevJump <- readIORef $ nPrevJump node
                               return $ Just $ ProgramState st prevJump pointer

genST chars = do ops <- sequence $ nodify . charToOpt <$> chars
                 void $ setNext ops
                 sequence_ $ setSelf <$> ops
                 sequence_ $ setNextJump <$> ops
                 sequence_ $ setPrevJump <$> ops
                 return $ head ops

printST node@(Node {..}) = do
    next <- readIORef nNext
    case next of
        Nothing -> print nOp
        Just n -> do
            print nOp
            self <- readIORef $ nSelf
            nextJump <- readIORef $ nNextJump
            prevJump <- readIORef $ nPrevJump
            when (isJust self) $ print "self set"
            when (isJust nextJump) $ print "next jump set"
            when (isJust prevJump) $ print "prev jump set"
            printST n

nodify nOp = do
              nPrevJump <- newIORef Nothing
              nNextJump <- newIORef Nothing
              nNext     <- newIORef Nothing
              nSelf     <- newIORef Nothing
              return Node {..}

setSelf n@(Node{..}) = do let s = nSelf
                          writeIORef s $ Just n
                          return n

setNext [] = return []
setNext [n] = return [n]
setNext (n1:n2:ns) = do void $ writeIORef (nNext n1) $ Just n2
                        setNext $ n2 : ns

setNextJump node = case nOp node of
    JumpForward -> do next <- readIORef $ nNext node
                      nextJump <- setNextJump' next 1
                      void $ writeIORef (nNextJump node) nextJump
                      return node
    _ -> return node

setNextJump' mb_node x = case mb_node of
    Nothing -> error "impossible6: unbalanced jumps"
    Just node -> do
            nxt <- readIORef $ nNext node
            case nxt of
                Nothing -> case nOp node of
                    JumpBack -> if x == 1
                                then return $ Just node
                                else error "impossible4: unbalanced jumps"
                    _ -> error   "impossible5: unbalanced jumps"
                Just next -> case nOp node of
                    JumpBack -> if x == 1
                                then return $ Just node
                                else setNextJump' (Just next) (x - 1)
                    JumpForward -> setNextJump' (Just next) (x + 1)
                    _ -> setNextJump' (Just next) x

setPrevJump node = case nOp node of
    JumpForward -> do mbNextJump <- readIORef $ nNextJump node
                      case mbNextJump of
                          Nothing -> error "impossible7 unbalanced jumps"
                          Just nextJump -> do
                              void $ writeIORef (nPrevJump nextJump) $ Just node
                      return node
    _ -> return node

data Node = Node
    { nPrevJump :: IORef (Maybe Node)
    , nNextJump :: IORef (Maybe Node)
    , nNext     :: IORef (Maybe Node)
    , nSelf     :: IORef (Maybe Node)
    , nOp       :: Op
    }

charToOpt c = case c of
    'a' -> JumpForward
    ';' -> JumpBack
    's' -> IncrementDataPointer
    'd' -> DecrementDataPointer
    'f' -> InputByte
    'j' -> OutputByte
    'k' -> IncrementByte
    'l' -> DecrementByte

data Op = IncrementDataPointer
        | DecrementDataPointer
        | IncrementByte
        | DecrementByte
        | InputByte
        | OutputByte
        | JumpForward
        | JumpBack
  deriving Show

homerowFile = do code <- concat <$> many1 line
                 eof
                 return code

line = do code <- sepBy (many (oneOf "asdfjkl;")) (many1 $ oneOf " \t")
          optional comment
          char '\n'
          return code

comment = oneOf "#-\"" >> many (noneOf "\n")

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
