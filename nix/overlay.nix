final:
previous:
with final.haskell.lib;
{
  yamlparsePackages =
    let
      yamlparsePkg = name:
        doBenchmark (failOnAllWarnings (final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}));
    in
      final.lib.genAttrs [
        "yamlparse-applicative"
        "yamlparse-applicative-demo"
      ] yamlparsePkg;

  haskellPackages = previous.haskellPackages.override (
    old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
        self: super: final.yamlparsePackages
      );
    }
  );
}
