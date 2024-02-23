module HW5.Pretty
  ( prettyValue
  , prettyParseError
  , prettyEvalError
  ) where

import Prettyprinter (Doc, pretty, (<+>), encloseSep, annotate, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle, color, Color (Magenta, Blue, Red, Cyan, Green))

import Data.Void (Void)
import Data.Scientific (fromRationalRepetendUnlimited)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import GHC.Real (Ratio ((:%)))
import Data.Ratio (numerator, denominator)
import Data.Foldable (toList)
import Numeric (showHex)
import Data.Text as T
import qualified Data.ByteString as B
import Data.Word (Word8)
import qualified Data.Map as Map

import HW5.Base


prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction HiFunDiv)            = pretty "div"
prettyValue (HiValueFunction HiFunMul)            = pretty "mul"
prettyValue (HiValueFunction HiFunAdd)            = pretty "add"
prettyValue (HiValueFunction HiFunSub)            = pretty "sub"
prettyValue (HiValueFunction HiFunNot)            = pretty "not"
prettyValue (HiValueFunction HiFunAnd)            = pretty "and"
prettyValue (HiValueFunction HiFunOr)             = pretty "or"
prettyValue (HiValueFunction HiFunLessThan)       = pretty "less-than"
prettyValue (HiValueFunction HiFunGreaterThan)    = pretty "greater-than"
prettyValue (HiValueFunction HiFunEquals)         = pretty "equals"
prettyValue (HiValueFunction HiFunNotLessThan)    = pretty "not-less-than"
prettyValue (HiValueFunction HiFunNotGreaterThan) = pretty "not-greater-than"
prettyValue (HiValueFunction HiFunNotEquals)      = pretty "not-equals"
prettyValue (HiValueFunction HiFunIf)             = pretty "if"
prettyValue (HiValueNumber num@(n :% d))          = case d of
                                                      1 -> magenta $ pretty n
                                                      _ -> magenta $ prettyRational num
prettyValue (HiValueBool value)                   = blue $ pretty $ if value then "true" else "false"
prettyValue (HiValueFunction HiFunLength)         = pretty "length"
prettyValue (HiValueFunction HiFunToUpper)        = pretty "to-upper"
prettyValue (HiValueFunction HiFunToLower)        = pretty "to-lower"
prettyValue (HiValueFunction HiFunReverse)        = pretty "reverse"
prettyValue (HiValueFunction HiFunTrim)           = pretty "trim"
prettyValue (HiValueString s)                     = cyan $ viaShow s
prettyValue (HiValueNull)                         = pretty "null"
prettyValue (HiValueFunction HiFunList)           = pretty "list"
prettyValue (HiValueFunction HiFunRange)          = pretty "range"
prettyValue (HiValueFunction HiFunFold)           = pretty "fold"
prettyValue (HiValueList s)                       = encloseSep (pretty "[ ") 
                                                      (pretty " ]") 
                                                      (pretty ", ") 
                                                      (toList $ prettyValue <$> s)
prettyValue (HiValueBytes bytes)                  = prettyBytes bytes
prettyValue (HiValueFunction HiFunPackBytes)      = pretty "pack-bytes"
prettyValue (HiValueFunction HiFunUnpackBytes)    = pretty "unpack-bytes"
prettyValue (HiValueFunction HiFunEncodeUtf8)     = pretty "encode-utf8"
prettyValue (HiValueFunction HiFunDecodeUtf8)     = pretty "decode-utf8"
prettyValue (HiValueFunction HiFunZip)            = pretty "zip"
prettyValue (HiValueFunction HiFunUnzip)          = pretty "unzip"
prettyValue (HiValueFunction HiFunSerialise)      = pretty "serialise"
prettyValue (HiValueFunction HiFunDeserialise)    = pretty "deserialise"
prettyValue (HiValueFunction HiFunRead)           = pretty "read"
prettyValue (HiValueFunction HiFunWrite)          = pretty "write"
prettyValue (HiValueFunction HiFunMkDir)          = pretty "mkdir"
prettyValue (HiValueFunction HiFunChDir)          = pretty "cd"
prettyValue (HiValueAction (HiActionRead path))   = prettyAction "read" "" [prettyString path]
prettyValue (HiValueAction (HiActionWrite to b))  = prettyAction "write" "" [prettyString to, prettyBytes b]
prettyValue (HiValueAction (HiActionMkDir path))  = prettyAction "mkdir" "" [prettyString path]
prettyValue (HiValueAction (HiActionChDir path))  = prettyAction "cd" "" [prettyString path]
prettyValue (HiValueAction HiActionCwd)           = pretty "cwd"
prettyValue (HiValueFunction HiFunParseTime)      = pretty "parse-time"
prettyValue (HiValueAction HiActionNow)           = pretty "now"
prettyValue (HiValueTime time)                    = prettyAction "parse-time" "" [viaShow $ show time]
prettyValue (HiValueFunction HiFunRand)           = pretty "rand"
prettyValue (HiValueAction (HiActionRand a b))    = prettyAction "rand" " " [pretty a, pretty b]
prettyValue (HiValueFunction HiFunEcho)           = pretty "echo"
prettyValue (HiValueAction (HiActionEcho s))      = prettyAction "echo" "" [viaShow $ s]
prettyValue (HiValueFunction HiFunCount)          = pretty "count"
prettyValue (HiValueFunction HiFunKeys)           = pretty "keys"
prettyValue (HiValueFunction HiFunValues)         = pretty "values"
prettyValue (HiValueFunction HiFunInvert)         = pretty "invert"
prettyValue (HiValueDict dict)                    = encloseSep (pretty "{ ")
                                                      (pretty " }")
                                                      (pretty ", ")
                                                      (prettyEntry <$> Map.toList dict)
                                                       


prettyRational :: Rational -> Doc AnsiStyle
prettyRational num@(n :% d) = let (scientific, repetend) = fromRationalRepetendUnlimited num in
  case repetend of
    Nothing -> pretty $ show scientific
    Just _ -> prettyPrefix <> pretty (abs $ numerator ratio) <> pretty '/' <> pretty (denominator ratio)
      where
        (quotient, remainder) = quotRem n d
        ratio = remainder :% d
        (minus, sign) = if remainder > 0 then ("", "+ ") else ("-", "- ")
        prettyPrefix = if quotient /= 0 then pretty quotient <+> pretty sign else pretty minus

prettyBytes :: B.ByteString -> Doc AnsiStyle
prettyBytes bytes = encloseSep (pretty "[# ")
  (pretty " #]")
  (pretty " ")
  (green . prettyByte <$> B.unpack bytes)
        
prettyByte :: Word8 -> Doc AnsiStyle
prettyByte b = pretty $ T.justifyRight 2 '0' $ T.pack (showHex b "")

prettyAction :: String -> String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyAction f ws args = pretty f <> encloseSep (pretty "(" <> pretty ws)
  (pretty ws <> pretty ")")
  (pretty ", ")
  args

prettyString :: String -> Doc AnsiStyle
prettyString = (pretty "\"" <>) . (<> pretty "\"") . pretty

prettyEntry :: (HiValue, HiValue) -> Doc AnsiStyle
prettyEntry (a, b) = prettyValue a <> pretty ": " <> prettyValue b 

prettyParseError :: (ParseErrorBundle String Void) -> Doc AnsiStyle
prettyParseError = pretty . errorBundlePretty

prettyEvalError :: HiError -> Doc AnsiStyle
prettyEvalError HiErrorInvalidArgument = red $ pretty "invalid argument"
prettyEvalError HiErrorInvalidFunction = red $ pretty "invalid function"
prettyEvalError HiErrorArityMismatch   = red $ pretty "arity mismatch"
prettyEvalError HiErrorDivideByZero    = red $ pretty "division by zero"

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Magenta)

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Cyan)

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Blue)
