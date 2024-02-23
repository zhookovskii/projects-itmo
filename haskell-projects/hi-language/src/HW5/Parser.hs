module HW5.Parser
  ( parse
  ) where

import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec (Parsec, runParser, empty, choice, eof, try, sepBy, optional, 
                        notFollowedBy, manyTill, many, satisfy, sepBy1, (<?>), (<|>))
import Text.Megaparsec.Char
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Scientific
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, pack)
import qualified Data.ByteString as B
import Data.Word (Word8)

import HW5.Base

type Parser = Parsec Void String

pWS :: Parser ()
pWS = L.space space1 empty empty

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pWS *> pHiExpr <* eof) []

pHiExpr :: Parser HiExpr
pHiExpr = makeExprParser pHiExprTerm operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = 
  [ [ binaryL "/" HiFunDiv, binaryL "*" HiFunMul ]
  , [ binaryL "+" HiFunAdd, binaryL "-" HiFunSub ]
  , [ binaryN "<" HiFunLessThan, binaryN ">" HiFunGreaterThan
    , binaryN "<=" HiFunNotGreaterThan, binaryN ">=" HiFunNotLessThan
    , binaryN "==" HiFunEquals, binaryN "/=" HiFunNotEquals ]
  , [ binaryR "&&" HiFunAnd ]
  , [ binaryR "||" HiFunOr ]
  ]

binaryL, binaryR, binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryL = (InfixL .) . binary 
binaryN = (InfixN .) . binary
binaryR = (InfixR .) . binary

binary :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
binary op f = (mkOp f) <$ (try $ pOperator op)
  
mkOp :: HiFun -> HiExpr -> HiExpr -> HiExpr
mkOp f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

pOperator :: String -> Parser String
pOperator op = pWS *> string op <* notFollowedBy (char '=') <* pWS

pHiExprTerm :: Parser HiExpr
pHiExprTerm = choice 
  [ try pHiExprApplyOrRun
  , pHiExprValue
  , paren pHiExpr
  ]

pHiExprApply :: Parser HiExpr
pHiExprApply = do
  f <- (pHiExprValue <|> paren pHiExpr) <* pWS
  case f of
    HiExprValue (HiValueAction _) -> return f
    _                             -> pHiExprApplyCont f
  
pHiExprApplyCont :: HiExpr -> Parser HiExpr
pHiExprApplyCont f = do
  args <- paren pArgs <|> pDotCall
  let expr = HiExprApply f args in do
    cont <- optional (pHiExprApplyCont expr)
    case cont of
      Just res -> return res
      Nothing  -> return expr
    
pHiExprApplyOrRun :: Parser HiExpr
pHiExprApplyOrRun = do
  expr <- pHiExprApply
  run <- optional $ char '!'
  case run of
    Just _  -> return $ HiExprRun expr
    Nothing -> return expr 
  

pHiExprValue :: Parser HiExpr
pHiExprValue = (HiExprValue <$> pHiValue) <|> try pList <|> pBytes <|> pDict

pArgs :: Parser [HiExpr]
pArgs = sepBy (pWS *> pHiExpr <* pWS) (char ',')

pHiValue :: Parser HiValue
pHiValue = choice 
  [ HiValueAction HiActionCwd <$ string "cwd" <* pWS
  , HiValueAction HiActionNow <$ try (string "now") <* pWS 
  , HiValueNumber             <$> pNumber <* pWS <?> "number"
  , HiValueFunction           <$> pHiFun <* pWS  <?> "function"
  , HiValueBool               <$> pBool <* pWS   <?> "boolean"
  , HiValueNull               <$  string "null" <* pWS
  , HiValueString             <$> pString <* pWS <?> "string"
  ]

pNumber :: Parser Rational
pNumber = toRational <$> pScientific

pHiFun :: Parser HiFun
pHiFun = choice
  [ HiFunDiv            <$ string "div"
  , HiFunMul            <$ string "mul"
  , HiFunAdd            <$ string "add"
  , HiFunSub            <$ string "sub"
  , HiFunAnd            <$ string "and"
  , HiFunOr             <$ string "or"
  , HiFunLessThan       <$ string "less-than"
  , HiFunGreaterThan    <$ string "greater-than"
  , HiFunEquals         <$ string "equals"
  , HiFunNotLessThan    <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals      <$ string "not-equals"
  , HiFunNot            <$ string "not"
  , HiFunIf             <$ string "if"
  , HiFunLength         <$ string "length"
  , HiFunToUpper        <$ string "to-upper"
  , HiFunToLower        <$ string "to-lower"
  , HiFunReverse        <$ string "reverse"
  , HiFunTrim           <$ string "trim"
  , HiFunList           <$ string "list"
  , HiFunRange          <$ string "range"
  , HiFunFold           <$ string "fold"
  , HiFunPackBytes      <$ string "pack-bytes"
  , HiFunUnpackBytes    <$ string "unpack-bytes"
  , HiFunEncodeUtf8     <$ string "encode-utf8"
  , HiFunDecodeUtf8     <$ string "decode-utf8"
  , HiFunZip            <$ string "zip"
  , HiFunUnzip          <$ string "unzip"
  , HiFunSerialise      <$ string "serialise"
  , HiFunDeserialise    <$ string "deserialise"
  , HiFunRead           <$ string "read"
  , HiFunWrite          <$ string "write"
  , HiFunMkDir          <$ string "mkdir"
  , HiFunChDir          <$ string "cd"
  , HiFunParseTime      <$ string "parse-time"
  , HiFunRand           <$ string "rand"
  , HiFunEcho           <$ string "echo"
  , HiFunCount          <$ string "count"
  , HiFunKeys           <$ string "keys"
  , HiFunValues         <$ string "values"
  , HiFunInvert         <$ string "invert"
  ]
  
pList :: Parser HiExpr
pList = wrapList <$> ((char '[') *> pArgs <* (char ']') <* pWS)
    
pBytes :: Parser HiExpr
pBytes = wrapBytes <$> ((string "[#") *> pWS *> many pByte <* (string "#]") <* pWS)
    
pDict :: Parser HiExpr
pDict = HiExprDict <$> ((char '{') *> pWS *> sepBy (pWS *> pEntry <* pWS) (char ',') <* pWS <* (char '}') <* pWS)
  
pEntry :: Parser (HiExpr, HiExpr)
pEntry = do
  k <- pHiExpr <* pWS <* (char ':') <* pWS
  v <- pHiExpr <* pWS
  return (k, v)
  
pByte :: Parser Word8
pByte = L.hexadecimal <* pWS
    
wrapList :: [HiExpr] -> HiExpr
wrapList = HiExprApply (HiExprValue $ HiValueFunction HiFunList)

wrapBytes :: [Word8] -> HiExpr
wrapBytes = HiExprValue . HiValueBytes . B.pack
  
paren :: Parser a -> Parser a
paren p = pWS *> char '(' *> pWS *> p <* pWS <* char ')' <* pWS  

pBool :: Parser Bool
pBool = (True <$ string "true") <|> (False <$ string "false")

pScientific :: Parser Scientific
pScientific = L.signed pWS L.scientific

pString :: Parser Text
pString = pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

pIdentifier :: Parser HiValue
pIdentifier = (HiValueString . pack . intercalate "-") <$> 
  ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  
pDotCall :: Parser [HiExpr]
pDotCall = do
  call <- char '.' *> pIdentifier
  return [HiExprValue call]
