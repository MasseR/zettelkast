with (import <nixpkgs> {});

rec {
  zettelkast = callPackage ./default.nix {};
  shell = buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with haskellPackages; [
      (ghcWithPackages (_: zettelkast.buildInputs ++ zettelkast.propagatedBuildInputs))
      ghcid
      cabal-install
      pkgs.binutils
      stylish-haskell
      entr
    ];
  };
}
