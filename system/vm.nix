{
  inputs,
  ...
}: {
  imports = [
    ./with/hardware/vm.nix
    ./with/nix.nix
    ./with/trait/allow-unfree-packages.nix
    ./with/trait/ssh-enabled.nix
    ./with/trait/has-swapfile.nix
    {
      has-swapfile.sizeGb = 4;
    }
    #./with/desktop-environment/gnome.nix
    ./with/user/root.nix
    ./with/user/me.nix
    {
      environment.systemPackages = [ inputs.agenix.packages.aarch64-linux.default];
    }
  ];
}
