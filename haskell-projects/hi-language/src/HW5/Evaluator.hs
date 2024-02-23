module HW5.Evaluator
  ( eval
  ) where
  
import HW5.Base

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad (foldM)

import GHC.Real (Ratio ((:%)))
import Data.Semigroup (stimes)
import Data.Text as T
import Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Text.Read (readMaybe)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Codec.Compression.Zlib (compressWith, defaultCompressParams, compressLevel, bestCompression, decompress)
import Codec.Serialise (serialise, deserialiseOrFail)
import Control.Monad.Trans.Class (lift)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExcept

evalExcept :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExcept (HiExprValue value) = pure value
evalExcept (HiExprApply funVal argsVal) = do
   f <- evalExcept funVal
   case f of
     HiValueFunction checkedF -> do
       evalApplyLazy checkedF (getArity argsVal)
     HiValueString str  -> do
       args <- mapM evalExcept argsVal 
       evalSlicing (HiStr $ T.unpack str) (getArity args)
     HiValueList sq     -> do
       args <- mapM evalExcept argsVal 
       evalSlicing (HiSeq $ toList sq) (getArity args)
     HiValueBytes bytes -> do
       args <- mapM evalExcept argsVal 
       evalSlicing (HiBytes $ B.unpack bytes) (getArity args)
     HiValueDict dict   -> do
       args <- mapM evalExcept argsVal  
       evalGetKey dict (getArity args)
     _                  -> throwE HiErrorInvalidFunction

evalExcept (HiExprRun expr)  = do
  actionVal <- evalExcept expr
  case actionVal of
    HiValueAction action -> lift $ runAction action
    _                    -> throwE HiErrorInvalidFunction
evalExcept (HiExprDict pairsExpr) = do
  pairs <- mapM evalPair pairsExpr
  return $ HiValueDict $ Map.fromList pairs

evalPair :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evalPair (a, b) = do
  first <- evalExcept a
  second <- evalExcept b
  return (first, second)

getArity :: [a] -> HiArity a
getArity [first, second, third] = HiTernary first second third
getArity [first, second]        = HiBinary first second
getArity [only]                 = HiUnary only 
getArity lst                    = HiArbitraryArity (Seq.fromList lst) 

-- utility constructors defined to assist in separating evaluation of different data types
data Arithmetic = Div | Mul | Add | Sub

data BooleanUnary = Not

data BooleanBinary = And | Or

data Comparison = Less | Greater | Equals | NotLess | NotGreater | NotEquals

data Branching = If

data Strings = ToUpper | ToLower | Trim

data Shared = Length | Reverse | Count

data HiList = HiSeq [HiValue] | HiStr [Char] | HiBytes [Word8]

data IOUnary = IORead | IOMkDir | IOChDir

data IOBinary = IOWrite

data MapEntries = Keys | Values

evalApplyLazy :: HiMonad m => HiFun -> HiArity HiExpr -> ExceptT HiError m HiValue
evalApplyLazy HiFunAnd (HiBinary first second) = do
  left <- evalExcept first
  case left of
    HiValueNull       -> return left
    HiValueBool False -> return left
    _                 -> evalExcept second
evalApplyLazy HiFunOr (HiBinary first second)  = do
  left <- evalExcept first
  case left of
    HiValueNull       -> evalExcept second
    HiValueBool False -> evalExcept second
    _                 -> return left
evalApplyLazy HiFunIf (HiTernary first thenDo elseDo) = do
  condExpr <- evalExcept first
  case condExpr of
    HiValueBool cond -> if cond then evalExcept thenDo else evalExcept elseDo
    _                -> throwE HiErrorInvalidArgument

evalApplyLazy f (HiUnary only) = evalExcept only >>= evalApply f . HiUnary
evalApplyLazy f (HiBinary first second) = mapM evalExcept [first, second] >>= evalApply f . getArity
evalApplyLazy f (HiTernary first second third) = mapM evalExcept [first, second, third] >>= evalApply f . getArity
evalApplyLazy f (HiArbitraryArity sq) = mapM evalExcept sq >>= evalApply f . getArity . toList

    
evalApply :: Monad m => HiFun -> HiArity HiValue -> ExceptT HiError m HiValue
evalApply HiFunDiv (HiBinary first second)            = evalArithmetic Div first second
evalApply HiFunMul (HiBinary first second)            = evalArithmetic Mul first second
evalApply HiFunAdd (HiBinary first second)            = evalArithmetic Add first second
evalApply HiFunSub (HiBinary first second)            = evalArithmetic Sub first second
evalApply HiFunNot (HiUnary only)                     = evalBooleanUnary Not only
evalApply HiFunAnd (HiBinary first second)            = evalBooleanBinary And first second
evalApply HiFunOr (HiBinary first second)             = evalBooleanBinary Or first second
evalApply HiFunLessThan (HiBinary first second)       = evalComparison Less first second
evalApply HiFunGreaterThan (HiBinary first second)    = evalComparison Greater first second
evalApply HiFunEquals (HiBinary first second)         = evalComparison Equals first second
evalApply HiFunNotLessThan (HiBinary first second)    = evalComparison NotLess first second
evalApply HiFunNotGreaterThan (HiBinary first second) = evalComparison NotGreater first second
evalApply HiFunNotEquals (HiBinary first second)      = evalComparison NotEquals first second
evalApply HiFunIf (HiTernary first second third)      = evalBranching If first second third
evalApply HiFunLength (HiUnary only)                  = evalShared Length only
evalApply HiFunToUpper (HiUnary only)                 = evalStrings ToUpper only
evalApply HiFunToLower (HiUnary only)                 = evalStrings ToLower only
evalApply HiFunReverse (HiUnary only)                 = evalShared Reverse only
evalApply HiFunTrim (HiUnary only)                    = evalStrings Trim only
evalApply HiFunList elements                          = pure $ HiValueList $ mkList elements
evalApply HiFunRange (HiBinary first second)          = evalRange first second
evalApply HiFunFold (HiBinary first second)           = evalFold first second
evalApply HiFunPackBytes (HiUnary only)               = evalPackBytes only
evalApply HiFunUnpackBytes (HiUnary only)             = evalUnpackBytes only
evalApply HiFunEncodeUtf8 (HiUnary only)              = evalEncodeUtf8 only
evalApply HiFunDecodeUtf8 (HiUnary only)              = evalDecodeUtf8 only
evalApply HiFunZip (HiUnary only)                     = evalZip only
evalApply HiFunUnzip (HiUnary only)                   = evalUnzip only
evalApply HiFunSerialise (HiUnary only)               = evalSerialise only
evalApply HiFunDeserialise (HiUnary only)             = evalDeserialise only
evalApply HiFunRead (HiUnary only)                    = evalIOUnary IORead only
evalApply HiFunWrite (HiBinary first second)          = evalIOBinary IOWrite first second
evalApply HiFunMkDir (HiUnary only)                   = evalIOUnary IOMkDir only
evalApply HiFunChDir (HiUnary only)                   = evalIOUnary IOChDir only
evalApply HiFunParseTime (HiUnary only)               = evalParseTime only
evalApply HiFunRand (HiBinary first second)           = evalRand first second
evalApply HiFunEcho (HiUnary only)                    = evalEcho only
evalApply HiFunCount (HiUnary only)                   = evalShared Count only
evalApply HiFunKeys (HiUnary only)                    = evalKeysValues Keys only
evalApply HiFunValues (HiUnary only)                  = evalKeysValues Values only
evalApply HiFunInvert (HiUnary only)                  = evalInvert only
evalApply _ _                                         = throwE HiErrorArityMismatch
    
evalArithmetic :: Monad m => Arithmetic -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalArithmetic f (HiValueNumber a) (HiValueNumber b) = do
  guardDivision f b
  return $ deriveArithmetic f a b
evalArithmetic Add (HiValueString a) (HiValueString b)            = return $ HiValueString $ a <> b
evalArithmetic Add (HiValueList a) (HiValueList b)                = return $ HiValueList $ a >< b
evalArithmetic Add (HiValueBytes a) (HiValueBytes b)              = return $ HiValueBytes $ B.append a b
evalArithmetic Add (HiValueTime time) (HiValueNumber diff)        = 
  return $ HiValueTime $ addUTCTime (fromRational diff) time
evalArithmetic Sub (HiValueTime a) (HiValueTime b)                =
  return $ HiValueNumber $ toRational $ diffUTCTime a b 
evalArithmetic Mul (HiValueString a) (HiValueNumber (times :% 1)) = return $ HiValueString $ stimes times a
evalArithmetic Mul (HiValueList a) (HiValueNumber (times :% 1))   = return $ HiValueList $ stimes times a
evalArithmetic Mul (HiValueBytes a) (HiValueNumber (times :% 1))  = return $ HiValueBytes $ stimes times a
evalArithmetic Div (HiValueString a) (HiValueString b)            = return $ HiValueString $ (T.snoc a '/') <> b
evalArithmetic _ _ _                                              = throwE HiErrorInvalidArgument
    
evalBooleanUnary :: Monad m => BooleanUnary -> HiValue -> ExceptT HiError m HiValue
evalBooleanUnary Not (HiValueBool value) = return $ HiValueBool $ not value
evalBooleanUnary _ _                     = throwE HiErrorInvalidArgument
    
evalBooleanBinary :: Monad m => BooleanBinary -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalBooleanBinary f (HiValueBool a) (HiValueBool b) = return $ deriveBoolean f a b
evalBooleanBinary _ _ _                             = throwE HiErrorInvalidArgument
    
evalComparison :: Monad m => Comparison -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalComparison f first second = return $ deriveComparison f first second

evalBranching :: Monad m => Branching -> HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalBranching If (HiValueBool cond) thenValue elseValue = if cond then return thenValue else return elseValue
evalBranching _ _ _ _                                   = throwE HiErrorInvalidArgument
    
evalStrings :: Monad m => Strings -> HiValue -> ExceptT HiError m HiValue
evalStrings f (HiValueString txt) = return $ deriveStrings f txt
evalStrings _ _                   = throwE HiErrorInvalidArgument

evalShared :: Monad m => Shared -> HiValue -> ExceptT HiError m HiValue
evalShared Length (HiValueString a)  = return $ HiValueNumber $ toRational $ T.length a
evalShared Length (HiValueList a)    = return $ HiValueNumber $ toRational $ Seq.length a
evalShared Length (HiValueBytes a)   = return $ HiValueNumber $ toRational $ B.length a
evalShared Reverse (HiValueString a) = return $ HiValueString $ T.reverse a
evalShared Reverse (HiValueList a)   = return $ HiValueList $ Seq.reverse a
evalShared Reverse (HiValueBytes a)  = return $ HiValueBytes $ B.reverse a
evalShared Count (HiValueString a)   = evalCount (HiStr $ T.unpack a) >>= return . HiValueDict
evalShared Count (HiValueList a)     = evalCount (HiSeq $ toList a) >>= return . HiValueDict
evalShared Count (HiValueBytes a)    = evalCount (HiBytes $ B.unpack a) >>= return . HiValueDict
evalShared _ _                       = throwE HiErrorInvalidArgument

evalCount :: Monad m => HiList -> ExceptT HiError m (Map.Map HiValue HiValue)
evalCount (HiSeq sq)      = return $ mkCounter sq
evalCount (HiStr txt)     = return $ Map.mapKeys (\c -> HiValueString $ T.pack [c]) $ mkCounter txt
evalCount (HiBytes bytes) = return $ Map.mapKeys (HiValueNumber . toRational) $ mkCounter bytes

mkCounter :: Ord a => [a] -> Map.Map a HiValue
mkCounter lst = Map.map HiValueNumber $ Prelude.foldl (\acc k -> Map.insertWith (+) k 1 acc) Map.empty lst

evalSlicing :: Monad m => HiList -> HiArity HiValue -> ExceptT HiError m HiValue
evalSlicing txt (HiUnary only)       = case only of
  HiValueNumber (idx :% 1) -> return $ findByIndex txt idx
  _                        -> throwE HiErrorInvalidArgument
evalSlicing txt (HiBinary start end) = mkSliceValue txt start end
evalSlicing _ _                      = throwE HiErrorArityMismatch

findByIndex :: HiList -> Integer -> HiValue
findByIndex hiList idx
  | (idx >= 0) && (toInt idx < hiListLen hiList) = case hiList of
    HiStr str -> HiValueString $ T.pack [(str !! (toInt idx))]
    HiSeq lst  -> lst !! (toInt idx)
    HiBytes bytes -> HiValueNumber $ fromIntegral $ bytes !! (toInt idx)
  | otherwise                                    = HiValueNull
  where
    hiListLen :: HiList -> Int
    hiListLen (HiStr str)     = len str
    hiListLen (HiSeq lst)     = len lst
    hiListLen (HiBytes bytes) = len bytes

mkSliceValue :: Monad m => HiList -> HiValue -> HiValue -> ExceptT HiError m HiValue
mkSliceValue hiList HiValueNull HiValueNull = case hiList of
  HiStr str     -> pure $ HiValueString $ T.pack str
  HiSeq lst     -> pure $ HiValueList $ Seq.fromList lst
  HiBytes bytes -> pure $ HiValueBytes $ B.pack bytes

mkSliceValue hiList HiValueNull (HiValueNumber (end :% 1)) = case hiList of
  HiStr str     -> return $ HiValueString $ T.pack $ mkSlice str 0 (toInt end)
  HiSeq lst     -> return $ HiValueList $ Seq.fromList $ mkSlice lst 0 (toInt end)
  HiBytes bytes -> return $ HiValueBytes $ B.pack $ mkSlice bytes 0 (toInt end)

mkSliceValue hiList (HiValueNumber (start :% 1)) HiValueNull = case hiList of
  HiStr str     -> return $ HiValueString $ T.pack $ mkSlice str (toInt start) (len str)
  HiSeq lst     -> return $ HiValueList $ Seq.fromList $ mkSlice lst (toInt start) (len lst)
  HiBytes bytes -> return $ HiValueBytes $ B.pack $ mkSlice bytes (toInt start) (len bytes)

mkSliceValue hiList (HiValueNumber (start :% 1)) (HiValueNumber (end :% 1)) = case hiList of
  HiStr str     -> return $ HiValueString $ T.pack $ mkSlice str (toInt start) (toInt end)
  HiSeq lst     -> return $ HiValueList $ Seq.fromList $ mkSlice lst (toInt start) (toInt end)
  HiBytes bytes -> return $ HiValueBytes $ B.pack $ mkSlice bytes (toInt start) (toInt end)

mkSliceValue _ _ _ = throwE HiErrorInvalidArgument

mkSlice :: [a] -> Int -> Int -> [a]
mkSlice lst start end = fst $ Prelude.foldl (\(pref, idx) ch ->
  if (startSlice <= idx && idx < endSlice) then
    (pref ++ [ch], idx + 1)
  else
    (pref, idx + 1)) ([], 0) lst
      where
        l          = len lst
        startSlice = if (start < 0) then l + start else start
        endSlice   = if (end < 0) then l + end else end

mkList :: HiArity HiValue -> Seq HiValue
mkList (HiUnary only)                 = Seq.singleton only
mkList (HiBinary first second)        = Seq.fromList [first, second]
mkList (HiTernary first second third) = Seq.fromList [first, second, third]
mkList (HiArbitraryArity sq)          = sq

evalRange :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalRange (HiValueNumber a) (HiValueNumber b) = return $ HiValueList $ Seq.fromList $ HiValueNumber <$> [a .. b]
evalRange _ _                                 = throwE HiErrorInvalidArgument

evalFold :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalFold (HiValueFunction f) (HiValueList (h :<| t)) = foldM (foldAcc f) h t
evalFold _ (HiValueList _)                           = pure $ HiValueNull
evalFold _ _                                         = throwE HiErrorInvalidArgument

foldAcc :: Monad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
foldAcc f x acc = evalApply f (HiBinary x acc)

evalPackBytes :: Monad m => HiValue -> ExceptT HiError m HiValue
evalPackBytes (HiValueList sq) = do
  checkedSq <- mapM (\val -> case val of
    HiValueNumber (n :% 1) -> if n >= 0 && n < 256 then return $ toByte n else throwE HiErrorInvalidArgument
    _                      -> throwE HiErrorInvalidArgument
    ) sq
  return $ HiValueBytes $ B.pack $ toList checkedSq
evalPackBytes _                = throwE HiErrorInvalidArgument

evalUnpackBytes :: Monad m => HiValue -> ExceptT HiError m HiValue
evalUnpackBytes (HiValueBytes sq) = return $ HiValueList $ Seq.fromList $ (HiValueNumber. toRational) <$> B.unpack sq
evalUnpackBytes _                 = throwE HiErrorInvalidArgument

evalEncodeUtf8 :: Monad m => HiValue -> ExceptT HiError m HiValue
evalEncodeUtf8 (HiValueString s) = return $ HiValueBytes $ encodeUtf8 s
evalEncodeUtf8 _                 = throwE HiErrorInvalidArgument

evalDecodeUtf8 :: Monad m => HiValue -> ExceptT HiError m HiValue
evalDecodeUtf8 (HiValueBytes bytes) = case decodeUtf8' bytes of
  Right str -> return $ HiValueString str
  Left _    -> return HiValueNull
evalDecodeUtf8 _                    = throwE HiErrorInvalidArgument

evalZip :: Monad m => HiValue -> ExceptT HiError m HiValue
evalZip (HiValueBytes bytes) = return $ HiValueBytes $ BL.toStrict
  $ compressWith defaultCompressParams { compressLevel = bestCompression } (BL.fromStrict bytes)
evalZip _                    = throwE HiErrorInvalidArgument

evalUnzip :: Monad m => HiValue -> ExceptT HiError m HiValue
evalUnzip (HiValueBytes bytes) = return $ HiValueBytes $ BL.toStrict $ decompress $ BL.fromStrict bytes
evalUnzip _                    = throwE HiErrorInvalidArgument

evalSerialise :: Monad m => HiValue -> ExceptT HiError m HiValue
evalSerialise = return . HiValueBytes . BL.toStrict . serialise

evalDeserialise :: Monad m => HiValue -> ExceptT HiError m HiValue
evalDeserialise (HiValueBytes bytes) = case deserialiseOrFail $ BL.fromStrict bytes of
  Right value -> return value
  Left _      -> return HiValueNull
evalDeserialise _                    = throwE HiErrorInvalidArgument

evalIOUnary :: Monad m => IOUnary -> HiValue -> ExceptT HiError m HiValue
evalIOUnary IORead (HiValueString path)  = pure $ HiValueAction $ HiActionRead $ T.unpack path
evalIOUnary IOMkDir (HiValueString path) = pure $ HiValueAction $ HiActionMkDir $ T.unpack path
evalIOUnary IOChDir (HiValueString path) = pure $ HiValueAction $ HiActionChDir $ T.unpack path
evalIOUnary _ _                          = throwE HiErrorInvalidArgument

evalIOBinary :: Monad m => IOBinary -> HiValue -> HiValue -> ExceptT HiError m HiValue
evalIOBinary IOWrite (HiValueString to) (HiValueString text) =
  pure $ HiValueAction $ HiActionWrite (T.unpack to) (encodeUtf8 text)
evalIOBinary _ _ _                                           = throwE HiErrorInvalidArgument

evalParseTime :: Monad m => HiValue -> ExceptT HiError m HiValue
evalParseTime (HiValueString s) = case readMaybeTime $ T.unpack s of
  Just time -> return $ HiValueTime time
  Nothing   -> return HiValueNull
  where
    readMaybeTime :: String -> Maybe UTCTime
    readMaybeTime = readMaybe

evalParseTime _                 = throwE HiErrorInvalidArgument

evalRand :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
evalRand (HiValueNumber (l :% 1)) (HiValueNumber (r :% 1)) = return $ HiValueAction $ HiActionRand (toInt l) (toInt r)
evalRand _ _                                               = throwE HiErrorInvalidArgument

evalEcho :: Monad m => HiValue -> ExceptT HiError m HiValue
evalEcho (HiValueString s) = return $ HiValueAction $ HiActionEcho s
evalEcho _                 = throwE HiErrorInvalidArgument

evalGetKey :: Monad m => Map.Map HiValue HiValue -> HiArity HiValue -> ExceptT HiError m HiValue
evalGetKey dict (HiUnary only) = case Map.lookup only dict of
  Just value -> return value
  Nothing    -> return HiValueNull
evalGetKey _ _                 = throwE HiErrorInvalidArgument

evalKeysValues :: Monad m => MapEntries -> HiValue -> ExceptT HiError m HiValue
evalKeysValues f (HiValueDict dict) = return $ HiValueList $ Seq.fromList $ case f of
  Keys -> Map.keys dict
  Values -> Map.elems dict
evalKeysValues _ _                  = throwE HiErrorInvalidArgument

evalInvert :: Monad m => HiValue -> ExceptT HiError m HiValue
evalInvert (HiValueDict dict) = return $ HiValueDict $ Map.map (HiValueList . Seq.fromList)
  $ Map.foldlWithKey (\acc k v -> Map.insertWith (++) v [k] acc) Map.empty dict
evalInvert _                  = throwE HiErrorInvalidArgument
    
guardDivision :: Monad m => Arithmetic -> Rational -> ExceptT HiError m ()
guardDivision Div 0 = throwE HiErrorDivideByZero
guardDivision _ _   = return ()
    
deriveArithmetic :: Arithmetic -> Rational -> Rational -> HiValue
deriveArithmetic Div = (HiValueNumber .) . (/)
deriveArithmetic Mul = (HiValueNumber .) . (*)
deriveArithmetic Add = (HiValueNumber .) . (+)
deriveArithmetic Sub = (HiValueNumber .) . (-)

deriveBoolean :: BooleanBinary -> Bool -> Bool -> HiValue
deriveBoolean And = (HiValueBool .) . (&&)
deriveBoolean Or  = (HiValueBool .) . (||)

deriveComparison :: Comparison -> HiValue -> HiValue -> HiValue
deriveComparison Less       = (HiValueBool .) . (<)
deriveComparison Greater    = (HiValueBool .) . (>)
deriveComparison Equals     = (HiValueBool .) . (==)
deriveComparison NotLess    = (HiValueBool .) . (>=)
deriveComparison NotGreater = (HiValueBool .) . (<=)
deriveComparison NotEquals  = (HiValueBool .) . (/=)

deriveStrings :: Strings -> T.Text -> HiValue
deriveStrings ToUpper = HiValueString . T.toUpper
deriveStrings ToLower = HiValueString . T.toLower
deriveStrings Trim    = HiValueString . T.strip

toInt :: Integer -> Int
toInt = fromIntegral

toByte :: Integer -> Word8
toByte = fromIntegral

len :: [a] -> Int
len = Prelude.length
