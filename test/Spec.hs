{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Flatten
import           Data.ByteString    (ByteString)
import           Data.Maybe         (fromJust)

decodeExample :: ByteString -> Value
decodeExample = fromJust . decodeStrict'

shouldResultIn :: ByteString -> ByteString -> Expectation
shouldResultIn = shouldResultInFn flatten

shouldResultIn' :: ByteString -> ByteString -> Expectation
shouldResultIn' = shouldResultInFn flatten'

shouldResultInFn :: (Value -> Value) -> ByteString -> ByteString -> Expectation
shouldResultInFn flattenFn input output = flattenFn (decodeExample input) `shouldBe` flattenFn (decodeExample output)

-- Test Cases

main :: IO ()
main = hspec $
  describe "Data.Aeson.Flatten" $ do
    describe "flatten" $ do
      it "works for empty object" $
        "{}" `shouldResultIn` "{}"
      it "works for simple object" $
        "{ \"a\": { \"b\": 1 } }" `shouldResultIn` "{ \"b\": 1 }"
      it "preserves values from more significant level in case of name clash" $
        "{ \"a\" : { \"b\": 1 }, \"b\": 2 }" `shouldResultIn` "{ \"b\": 2 }"
      it "works for one level flattening" $
        "{ \"a\": { \"b\": 1, \"c\": 2 } }" `shouldResultIn` "{ \"b\": 1, \"c\": 2 }"
      it "works for two level flattening" $
        "{ \"a\": { \"b\": { \"c\": 1 } } }" `shouldResultIn` "{ \"c\": 1 }"
      it "works for array with one level objects" $
        "{ \"a\": [ { \"b\": { \"c\": 1 } }, { \"b\": { \"c\": 234 } } ] }" `shouldResultIn` "{ \"a\": [ { \"c\": 1 }, { \"c\": 234 } ] }"

    describe "flatten'" $ do
      it "works for empty object" $
        "{}" `shouldResultIn'` "{}"
      it "works for simple object" $
        "{ \"a\": { \"b\": 1 } }" `shouldResultIn'` "{ \"a.b\": 1 }"
      it "preserves values where flatten would result in data loss" $
        "{ \"a\" : { \"b\": 1 }, \"b\": 2 }" `shouldResultIn'` "{ \"a.b\": 1, \"b\": 2 }"
      it "works for one level flattening" $
        "{ \"a\": { \"b\": 1, \"c\": 2 } }" `shouldResultIn'` "{ \"a.b\": 1, \"a.c\": 2 }"
      it "works for two level flattening" $
        "{ \"a\": { \"b\": { \"c\": 1 } } }" `shouldResultIn'` "{ \"a.b.c\": 1 }"
      it "works for array with one level objects" $
        "{ \"a\": [ { \"b\": { \"c\": 1 } }, { \"b\": { \"c\": 234 } } ] }" `shouldResultIn'` "{ \"a\": [ { \"b.c\": 1 }, { \"b.c\": 234 } ] }"
