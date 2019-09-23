let
  default = import ./default.nix;
  hsPkgs = default.hsPkgs;
  pkgs = default.pkgs;
in hsPkgs.shellFor {
  packages = ps: with ps; [
    parnix
  ];

  withHoogle = true;

  buildInputs = with pkgs.haskellPackages; [
    ghcid
  ];
}
