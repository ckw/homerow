{-# LANGUAGE RecordWildCards #-}

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import qualified System.IO.Error as E
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (unless, void )
import Control.Monad.Fix (fix)
import Data.IORef
import Data.List (partition)
import Data.Maybe
import Data.Sequence ((><), adjust)
import qualified Data.Sequence as S
import Data.Word

main :: IO ()
main = do (params, filename) <- second (return . head)
                              . partition (`elem` ["--homerow"]) <$> getArgs
          let (variant, parseF) = if "--homerow" `elem` params
                        then (fromHomerow, parseHR)
                        else (id, parseBF)
          input <- fmap variant $ readFile =<< filename
          let raw = case parseF input of
                           Left e -> show e
                           Right l -> concat l
          unless (isBalanced raw) $ error "unbalanced input"
          node <- genST raw
          let state = S.replicate 30000 0
              pointer = 0
              initial = ProgramState state (Just node) pointer
          flip fix initial $ \loop st -> do
              res <- step st
              case res of
                  Nothing -> return ()
                  Just ps -> loop ps

data ProgramState = ProgramState (S.Seq Word8) (Maybe Node) Pointer

instance Show ProgramState where
    show (ProgramState st node ptr) = (fromMaybe "NOP" $ show <$> node)
                  ++ ", ptr : "
                  ++ (show ptr)
                  ++ ", byte at ptr: "
                  ++ (show $ S.index st ptr )

type Pointer = Int

step :: ProgramState -> IO (Maybe ProgramState)
step ps@(ProgramState st mb_node pointer) = do
    --print ps
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
                    let tryReadChar = fromIntegral . fromEnum <$> hGetChar stdin
                    newByte <- tryReadChar `E.catchIOError`
                                   \e -> if E.isEOFError e
                                         then return 0
                                         else E.ioError e
                    let st' = S.update pointer newByte st
                    return $ Just $ ProgramState st' next pointer

                OutputByte -> do
                    next <- readIORef $ nNext node
                    hPutChar stdout (toEnum $ fromIntegral $ S.index st pointer)
                    return $ Just $ ProgramState st next pointer

                JumpForward -> do nextJump <- readIORef $ nNextJump node
                                  if S.index st pointer == 0
                                  then do
                                      case nextJump of
                                          Nothing -> return $ Just $ ProgramState st Nothing pointer
                                          Just a -> do
                                              after <- readIORef $ nNext a
                                              return $ Just $ ProgramState st after pointer
                                  else do
                                      next <- readIORef $ nNext node
                                      return $ Just $ ProgramState st next pointer
                JumpBack -> do prevJump <- readIORef $ nPrevJump node
                               if S.index st pointer == 0
                               then do n <- readIORef $ nNext node
                                       return $ Just $ ProgramState st n pointer
                               else case prevJump of
                                      Nothing -> return $ Just $ ProgramState st Nothing pointer
                                      Just prev -> do
                                          n <- readIORef $ nNext prev
                                          return $ Just $ ProgramState st n pointer

genST :: [Char] -> IO Node
genST chars = do ops <- sequence $ nodify . charToOpt <$> chars
                 void $ setNext ops
                 sequence_ $ setSelf <$> ops
                 sequence_ $ setNextJump <$> ops
                 sequence_ $ setPrevJump <$> ops
                 return $ head ops

fromHomerow str = convertChar <$> str
  where convertChar c = case c of
                            'a' -> '['
                            ';' -> ']'
                            's' -> '>'
                            'd' -> '<'
                            'f' -> ','
                            'j' -> '.'
                            'k' -> '+'
                            'l' -> '-'
                            _   -> c

nodify :: Op -> IO Node
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
                      nextJump <- setNextJump' next (1 :: Int)
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

instance Show Node where
    show = show . nOp

charToOpt c = case c of
    '[' -> JumpForward
    ']' -> JumpBack
    '>' -> IncrementDataPointer
    '<' -> DecrementDataPointer
    ',' -> InputByte
    '.' -> OutputByte
    '+' -> IncrementByte
    '-' -> DecrementByte
    c   -> error $ "impossible char: " ++ [c]

data Op = IncrementDataPointer
        | DecrementDataPointer
        | IncrementByte
        | DecrementByte
        | InputByte
        | OutputByte
        | JumpForward
        | JumpBack
  deriving Show


brainfuckFile = do code <- sepBy (many (oneOf "[]><,.+-")) (many1 $ noneOf "[]><,.+-")
                   eof
                   return code

homerowFile = do code <- concat <$> many1 line
                 eof
                 return code

line = do code <- sepBy (many (oneOf "[]><,.+-")) (many1 $ oneOf " \t")
          optional comment
          _ <- char '\n'
          return code

comment = oneOf "#-\"" >> many (noneOf "\n")

parseHR :: [Char] -> Either ParseError [[Char]]
parseHR input = parse homerowFile "so much fail" input

parseBF :: [Char] -> Either ParseError [[Char]]
parseBF input = parse brainfuckFile "so much fail" input

isBalanced :: String -> Bool
isBalanced input = case parse balance "jumps unbalanced" input of
                       Left _ -> False
                       Right _ -> True

balance' = do
    void nonJumps
    _ <- char '['
    void nonJumps
    void $ many balance'
    void nonJumps
    _ <- char ']'
    nonJumps

balance = many balance' >> eof

nonJumps = many $ oneOf "><,.+-"
