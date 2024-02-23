{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DerivingVia, InstanceSigs #-}

module HW5.Action
 ( HIO (..)
 , allowAll
 , HiPermission (..)
 , PermissionException (..)
 ) where

import HW5.Base
import Control.Exception.Base (Exception, throwIO)
import Data.Set (Set, notMember, fromList)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), MonadReader)
import Data.ByteString as B
import System.Directory (listDirectory, doesFileExist, createDirectory, setCurrentDirectory, getCurrentDirectory)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import System.Random (randomR, getStdGen, setStdGen)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)
  
data PermissionException =
  PermissionRequired HiPermission
  deriving (Show, Exception)

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (Set HiPermission) IO)
  deriving (MonadReader (Set HiPermission)) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction (HiActionRead path) = do
    requirePermission AllowRead
    fileExists <- liftIO $ doesFileExist path
    if fileExists then do
      content <- liftIO $ B.readFile path
      case decodeUtf8' content of
        Right str -> return $ HiValueString str
        Left _    -> return $ HiValueBytes content
    else do
      contents <- liftIO $ listDirectory path
      return $ HiValueList $ Seq.fromList $ HiValueString . T.pack <$> contents
      
  runAction (HiActionWrite path bytes) = requireAndRun AllowWrite (B.writeFile path bytes)
  runAction (HiActionMkDir path)       = requireAndRun AllowWrite (createDirectory path)
  runAction (HiActionChDir path)       = requireAndRun AllowRead (setCurrentDirectory path)
  runAction HiActionCwd                = do
    requirePermission AllowRead
    dir <- liftIO $ getCurrentDirectory
    return $ HiValueString $ T.pack $ dir
  runAction HiActionNow                = do
    requirePermission AllowTime
    time <- liftIO $ getCurrentTime
    return $ HiValueTime time
  runAction (HiActionRand left right)  = do
    gen <- getStdGen
    let (r, newGen) = randomR (left, right) gen
    setStdGen newGen
    return $ HiValueNumber $ toRational r
  runAction (HiActionEcho txt)         = requireAndRun AllowWrite (Prelude.putStrLn $ T.unpack txt)  
    
requireAndRun :: HiPermission -> IO () -> HIO HiValue
requireAndRun perm run = requirePermission perm >> (liftIO $ run) >> pure HiValueNull
    
requirePermission :: HiPermission -> HIO ()
requirePermission perm = HIO (\set -> if notMember perm set then throwIO $ PermissionRequired perm else return ())

allowAll :: Set HiPermission
allowAll = fromList [AllowRead, AllowWrite, AllowTime]
  