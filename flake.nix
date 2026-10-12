{
  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/*";

  outputs =
    { nixpkgs, ... }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      eachSystem =
        f: nixpkgs.lib.genAttrs supportedSystems (system: f { pkgs = nixpkgs.legacyPackages.${system}; });
    in
    {
      devShells = eachSystem (
        { pkgs }:
        {
          default = pkgs.mkShell {
            packages = with pkgs.ocamlPackages; [
              ocaml
              dune_3
              findlib
              ppx_deriving
              ocaml-lsp
              ocamlformat
              utop
            ];
          };
        }
      );
    };
}
