module Main (main) where
  
import System.Console.Haskeline

import HW5.Evaluator
import HW5.Parser
import HW5.Pretty
import HW5.Base
import HW5.Action

import Control.Monad.IO.Class (liftIO)
import Prettyprinter (Doc, line)
import Prettyprinter.Render.Terminal.Internal (AnsiStyle, putDoc)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just input -> do handleInput input
                                loop

handleInput :: String -> InputT IO ()
handleInput s = case parse s of
  Left err -> println $ prettyParseError err
  Right expr -> safeEval expr
  
safeEval :: HiExpr -> InputT IO ()
safeEval expr = do
  res <- liftIO $ runHIO (eval expr) allowAll
  case res of
    Left err -> println $ prettyEvalError err
    Right value -> println $ prettyValue value
    
println :: Doc AnsiStyle -> InputT IO ()
println doc = liftIO $ putDoc $ doc <> line