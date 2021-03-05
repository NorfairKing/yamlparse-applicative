{ pkgs ? import ./nix/pkgs.nix {} }:
let
  versions = [
    "lts-13_19"
    "lts-14_23"
    "lts-15_03"
    "lts-16_11"
    "lts-16_20"
  ];

  mkReleaseForVersion = version:
    let
      nixpkgsVersion = import (./ci + "/${version}.nix");
      pkgsf = import ./nix/nixpkgs.nix { inherit nixpkgsVersion; };
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in
      p.yamlparseApplicativeRelease;
in
{
  release = pkgs.yamlparseApplicativeRelease;
  pre-commit-check = (import ./default.nix).pre-commit-check;
} // pkgs.lib.genAttrs versions mkReleaseForVersion
