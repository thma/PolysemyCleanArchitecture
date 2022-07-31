module CleanArchitectureSpec where

import Test.Hspec
import DependencyChecker 

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "The Dependency Checker" $ do
    it "makes sure all modules comply to the outside-in rule" $ do
      allImports <- allImportDeclarations "src"
      verifyCleanArchitectureDependencies allImports `shouldBe` Right ()
    it "finds non-compliant import declarations" $ do
      allImports <- allImportDeclarations "src"
      verifyCleanArchitectureDependencies (allImports ++ [bogusDependency]) `shouldBe`
        Left [bogusDependency]

-- | this instance represents a non-compliant dependency from the 'Domain' package to the 'ExternalInterfaces' package.
bogusDependency :: (ModName, [Import])
bogusDependency = (mod, [imp])
  where
    mod =  (fromHierarchy ["Domain"],"ReservationDomain")
    imp = Import { impMod = (fromHierarchy ["ExternalInterfaces"],"FileConfigProvider"), impType = NormalImp }

