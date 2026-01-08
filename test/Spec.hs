import Test.Hspec
import Scheme.IO

main :: IO ()
main = hspec $ do
  describe "Scheme Interpreter" $ do
    describe "Parsing" $ do
      it "parses a number" $ do
        let result = readExpr "42"
        show result `shouldBe` "Right 42"

      it "parses a string" $ do
        let result = readExpr "\"hello\""
        show result `shouldBe` "Right \"hello\""

      it "parses an atom" $ do
        let result = readExpr "foo"
        show result `shouldBe` "Right foo"

      it "parses a list" $ do
        let result = readExpr "(+ 1 2)"
        show result `shouldBe` "Right (+ 1 2)"

    describe "Evaluation" $ do
      it "evaluates a simple addition" $ do
        env <- primitiveBindings
        result <- runIOThrows $ eval env (List [Atom "+", Number 1, Number 2]) >>= return . show
        result `shouldBe` "3"

      it "evaluates an if true" $ do
        env <- primitiveBindings
        result <- runIOThrows $ eval env (List [Atom "if", Bool True, Number 1, Number 2]) >>= return . show
        result `shouldBe` "1"

      it "evaluates an if false" $ do
        env <- primitiveBindings
        result <- runIOThrows $ eval env (List [Atom "if", Bool False, Number 1, Number 2]) >>= return . show
        result `shouldBe` "2"

      it "defines and uses a variable" $ do
        env <- primitiveBindings
        _ <- runIOThrows $ eval env (List [Atom "define", Atom "x", Number 5]) >>= return . show
        result <- runIOThrows $ eval env (Atom "x") >>= return . show
        result `shouldBe` "5"
