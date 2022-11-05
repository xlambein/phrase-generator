{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ pkgs.elmPackages.elm ];
        };
        
        packages.default = pkgs.callPackage ./. {};
        
        packages.update-elm = pkgs.writeShellScriptBin "update-elm" ''
            ${pkgs.elm2nix}/bin/elm2nix convert > elm-packages.nix
            ${pkgs.elm2nix}/bin/elm2nix snapshot > registry.dat
          '';
      }
  );
}
