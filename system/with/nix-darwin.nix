{...}: {
  nix = {
    linux-builder.enable = true;
    
    # Configure distributed builds to use both aarch64 and x86_64 remote builders
    distributedBuilds = true;
    
    buildMachines = [
      {
        hostName = "linux-builder";
        sshUser = "builder";
        sshKey = "/etc/nix/builder_ed25519";
        system = "aarch64-linux";
        maxJobs = 4;
        supportedFeatures = ["kvm" "benchmark" "big-parallel"];
      }
      {
        hostName = "linux-builder";
        sshUser = "builder"; 
        sshKey = "/etc/nix/builder_ed25519";
        system = "x86_64-linux";
        maxJobs = 2;
        supportedFeatures = ["kvm" "benchmark" "big-parallel"];
      }
    ];
    
    settings = {
      trusted-users = ["@admin"];
      # Use substitutes on remote builders to reduce build times
      builders-use-substitutes = true;
    };
  };
}
