{ inputs, outputs, nixpkgs, ... }: {
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.extraSpecialArgs = { inherit inputs outputs nixpkgs; };
}
