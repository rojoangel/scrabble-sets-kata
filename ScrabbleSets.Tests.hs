module ScrabbleSets.Test where
import ScrabbleSets (leftTiles)
import Control.Exception (evaluate)
import Test.Hspec

main = hspec $ do
  describe "leftTiles" $ do
    it "should work on the examples" $ do
       leftTiles "AEERTYOXMCNB_S" `shouldBe`    [(10,"E"),
                                                  (9,"I"),
                                                  (8,"A"),
                                                  (7,"O"),
                                                  (5,"NRT"),
                                                  (4,"DLU"),
                                                  (3,"GS"),
                                                  (2,"FHPVW"),
                                                  (1,"BCJKMQYZ_"),
                                                  (0,"X")]

       leftTiles "PQAREIOURSTHGWIOAE_" `shouldBe` [(10,"E"),
                                                  (7,"AI"),
                                                  (6,"NO"),
                                                  (5,"T"),
                                                  (4,"DLR"),
                                                  (3,"SU"),
                                                  (2,"BCFGMVY"),
                                                  (1,"HJKPWXZ_"),
                                                  (0,"Q")]

       leftTiles "LQTOONOEFFJZT" `shouldBe`       [(11,"E"),
                                                  (9,"AI"),
                                                  (6,"R"),
                                                  (5,"NO"),
                                                  (4,"DSTU"),
                                                  (3,"GL"),
                                                  (2,"BCHMPVWY_"),
                                                  (1,"KX"),
                                                  (0,"FJQZ")]

       evaluate(leftTiles "AXHDRUIOR_XHJZUQEE") `shouldThrow` errorCall "Invalid input. More X's have been taken from the bag than possible."
