module Main where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified System.Process as Proc

import Karabiner.Config
import qualified Karabiner.Config.Test.Data.Linux as Linux

main :: IO ()
main = hspec $ do
  describe "karabiner config" $ do
    it "linux example" $ do
      Linux.root `assertMatchesFixture` Linux.expectedJSON

assertMatchesFixture :: Root -> ByteString -> IO ()
assertMatchesFixture root s = do
  LC.toStrict (encodeRoot root) `assertEqualWithDiff` s

assertEqualWithDiff :: ByteString -> ByteString -> IO ()
assertEqualWithDiff actual expected = do
  when (actual /= expected) $ do
    getDiff actual expected >>= \case
      Nothing -> expectationFailure "comparison failed (but no diff command found)"
      Just diff -> expectationFailure $ "comparison failed (see diff below)\n" <> diff

{-# NOINLINE getDiffCommand #-}
getDiffCommand :: Maybe FilePath
getDiffCommand = unsafePerformIO $ do
  canColorDiff <- commandExists "colordiff"
  if canColorDiff then do
    pure $ Just "colordiff"
  else do
    canDiff <- commandExists "diff"
    if canDiff then do
      pure $ Just "diff"
    else do
      pure Nothing
  where
  commandExists :: FilePath -> IO Bool
  commandExists command = do
    (code, _, _) <- Proc.readProcessWithExitCode "command" ["-v", command] ""
    pure $ code == ExitSuccess

getDiff :: ByteString -> ByteString -> IO (Maybe String)
getDiff actual expected = case getDiffCommand of
  Nothing -> pure Nothing
  Just diffCommand -> do
    withSystemTempDirectory "karabiner-config-test" $ \tmpDir -> do
      let tmpActualPath = tmpDir <> "/actual.json"
          tmpExpectedPath = tmpDir <> "/expected.json"
      B.writeFile tmpActualPath actual
      B.writeFile tmpExpectedPath expected
      let p = (Proc.proc diffCommand ["-u", tmpActualPath, tmpExpectedPath]) {
                Proc.std_out = Proc.CreatePipe }
      Proc.withCreateProcess p $ \_ mOutH _ _ -> do
        let outH = fromMaybe (error "Failed to get stdout from diff") mOutH
        Just . BC.unpack <$> B.hGetContents outH
