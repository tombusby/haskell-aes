module PaddingSpec
  ( runTests,
  )
where

import Control.Exception (evaluate)
import Globals (Block, blockSize)
import Padding
import Test.Hspec
import Test.QuickCheck

needsPadding :: [Block]
needsPadding = [[0x6B, 0xC1, 0xBE, 0xE2, 0x2E, 0x40, 0x9F, 0x96]]

noPadding :: [Block]
noPadding =
  [ [ 0x6B,
      0xC1,
      0xBE,
      0xE2,
      0x2E,
      0x40,
      0x9F,
      0x96,
      0x6B,
      0xC1,
      0xBE,
      0xE2,
      0x2E,
      0x40,
      0x9F,
      0x96
    ]
  ]

emptyPaddingBlock :: Block
emptyPaddingBlock = [0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

runTests :: IO ()
runTests = hspec $
  describe "Padding Module" $ do
    describe "pad" $ do
      it "checks padding produces blocks of all length `blockSize`" $ do
        pad noPadding `shouldSatisfy` all ((== blockSize) . length)
        pad needsPadding `shouldSatisfy` all ((== blockSize) . length)
      it "checks padding produces correct output" $ do
        property $ \x -> case x of
          [] -> pad x == [emptyPaddingBlock]
          _ ->
            let p = pad x
             in if (length . last) x == blockSize
                  then last p == emptyPaddingBlock && init p == x
                  else
                    and . zipWith (==) (last p) $
                      last x ++ [0x80] ++ repeat 0
    describe "unpad" $ do
      it "checks that unpad is pad's inverse" $ do
        property checkPadAndUnpadAreInverse
      it "checks that invalid padding will not unpad" $ do
        unpad [[0, 0, 0, 0]] `shouldBe` Nothing
        unpad [[1, 2, 0x80, 3, 4]] `shouldBe` Nothing
        unpad [[1, 2, 0, 0, 0]] `shouldBe` Nothing
        unpad [[1, 2, 0x80, 0, 0]] `shouldNotBe` Nothing

checkPadAndUnpadAreInverse :: [Block] -> Bool
checkPadAndUnpadAreInverse [] = (unpad . pad) [] == Just []
checkPadAndUnpadAreInverse bs = (unpad . pad) bs' == Just bs'
  where
    -- Prevents issues caused when `last bs == []`
    -- (This is state that cannot occur naturally)
    bs' = filter (/= []) bs
