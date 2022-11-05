{ stdenv
, elmPackages
, lib
, nodePackages
}:

stdenv.mkDerivation {
  name = "phrase-generator";
  
  src = ./.;

  buildInputs = [ elmPackages.elm ];

  buildPhase = elmPackages.fetchElmDeps {
    elmPackages = import ./elm-packages.nix;
    elmVersion = "0.19.1";
    registryDat = ./registry.dat;
  };

  installPhase = ''
    elm make src/Main.elm --optimize --output $out/main.js
    cp index.html $out/
    cp style.css $out/
  '';
}
