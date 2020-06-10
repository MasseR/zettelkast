{ haskellPackages }:

haskellPackages.callCabal2nix "zettelkast" ./. {}
