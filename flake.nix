# {
#   description = "Dev shell with uv (and Python)";

#   inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

#   outputs = { self, nixpkgs }:
#   let
#     forAllSystems = f:
#       nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
#         f (import nixpkgs { inherit system; }));
#   in
#   {
#     devShells = forAllSystems (pkgs:
#       {
#         default = pkgs.mkShell {
#           packages = with pkgs; [
#             uv               # the fast Python package/project manager (Rust)
#             python312        # optional: provide a system Python (uv can also install its own)
#           ];

#           # Helpful defaults for Nix systems
#           shellHook = ''
#             # Prevent uv from fetching random CPython builds when you prefer nixpkgs' python
#             export UV_PYTHON_DOWNLOADS=never

#             # Make sure ~/.local/bin (where `uv tool install` puts shims) is on PATH
#             export PATH="$HOME/.local/bin:$PATH"

#             echo "Dev shell ready: uv=$(uv --version)  python=$(python --version)"
#           '';
#         };
#       });
#   };
# }

{
  description = "Dev shell with Elm";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
  let
    forAllSystems = f:
      nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
        f (import nixpkgs { inherit system; }));
  in
  {
    devShells = forAllSystems (pkgs:
      {
        default = pkgs.mkShell {
          packages = with pkgs; [
            elm2nix
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-review
            elmPackages.elm-test
            nodejs
          ];

          shellHook = ''
            echo "Elm dev shell ready!"
            echo "elm: $(elm --version)"
            echo "node: $(node --version)"
          '';
        };
      });
  };
}