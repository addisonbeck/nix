let
  special_nixos = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIITJNv1ewE1vKWYdXkpCmuQqvc0js217YB36FZq9qPMs nixos-dev-deployment-key";
  system_dev = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB7cJqsBFc+HoTRobNfNErjI8FOE8j7eF/4pw3sBxamL root@nix.addisonbeck.dev";

  specials = [ special_nixos ];
  systems = [ system_dev ];

  all = systems ++ specials;
in
{
  "github.age".publicKeys = all;
}
