{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , HiArity (..)
  , HiAction (..)
  , HiMonad (..)
  ) where
    
import Data.Text
import Data.Sequence (Seq)
import Data.Map (Map)
import Data.ByteString (ByteString)
import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Data.Time (UTCTime)


data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiValue =
    HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, Serialise)


-- strict typing for different function arity
data HiArity a =
    HiTernary a a a 
  | HiBinary a a 
  | HiUnary a
  | HiArbitraryArity (Seq a)
  deriving Show

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving Show

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving Show
  
data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic, Serialise)
  
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
