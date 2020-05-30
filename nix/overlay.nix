{
  hexOrganization,
  hexApiKey,
  robotSshKey
}:
self: super:
  let
    callPackage = self.lib.callPackageWith self.haskellPackages;
    dontCheck = self.haskell.lib.dontCheck;
    doJailbreak = self.haskell.lib.doJailbreak;
  in
    {
      haskellPackages = super.haskell.packages.ghc865.extend(
        self': super': {

        }
      );
    }
