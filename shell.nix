let
  packages = import ./.;
  inherit (packages) pkgs cardano-dex-sdk;
  inherit (cardano-dex-sdk) haskell;

in
  haskell.project.shellFor {

    shellHook = "
              export LC_CTYPE=C.UTF-8
              export LC_ALL=C.UTF-8
              export LANG=C.UTF-8
              ";

    withHoogle = false;

    nativeBuildInputs = with cardano-dex-sdk; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];
  }
