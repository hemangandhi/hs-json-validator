with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "hs-validator-env";
  buildInputs = [
    zig
  ];
}
