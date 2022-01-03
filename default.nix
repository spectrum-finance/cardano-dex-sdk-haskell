########################################################################
# default.nix -- The top-level nix build file for cardano-dex-sdk.
#
# This file defines various attributes that are used for building and
# developing cardano-dex-sdk.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   cardano-dex-sdk: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix;

  inherit (packages) pkgs cardano-dex-sdk;
  project = cardano-dex-sdk.haskell.project;
in
{
  inherit pkgs cardano-dex-sdk;

  inherit project;
}
