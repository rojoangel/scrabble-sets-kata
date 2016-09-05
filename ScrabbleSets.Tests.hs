module ScrabbleSets.Test where
import ScrabbleSets (tiles_left_in_bag)
import Test.Hspec

main = hspec $ do
  describe "group_tiles_by_count" $ do
    it "should work on the examples" $ do
      tiles_left_in_bag "AEERTYOXMCNB_S" `shouldBe` [ (10,"E"),
                                                      (9,"I"),
                                                      (8,"A"),
                                                      (7,"O"),
                                                      (5,"NRT"),
                                                      (4,"DLU"),
                                                      (3,"GS"),
                                                      (2,"FHPVW"),
                                                      (1,"BCJKMQYZ_"),
                                                      (0,"X")]
