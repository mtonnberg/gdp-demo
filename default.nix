let
  nixos_20_09 = import <nixos_20_09> { };
  unstable = import <nixpkgs> { };
in unstable.stdenv.mkDerivation rec {
  name = "gdp-demo";
  buildInputs = [
    nixos_20_09.ormolu
    nixos_20_09.hlint
    nixos_20_09.nodePackages.prettier
    nixos_20_09.python39
    unstable.stack
    unstable.postgresql_13
    unstable.haskellPackages.haskell-language-server
    unstable.haskellPackages.hspec-discover
  ];
  env = unstable.buildEnv {
    name = name;
    paths = buildInputs;
  };
  shellHook = ''
    git config core.hooksPath .hooks/
  '';
  LANG = ""; # This is required to build the project dependencies
}
