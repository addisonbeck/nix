let
  special_nixos = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIITJNv1ewE1vKWYdXkpCmuQqvc0js217YB36FZq9qPMs nixos-dev-deployment-key";
  system_dev = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB7cJqsBFc+HoTRobNfNErjI8FOE8j7eF/4pw3sBxamL root@nix.addisonbeck.dev";
  user_me_at_dev = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAnXcxUqpSAzi6b9Eao/hWYYpx/BACBFPyAFxnOW65bw me@nix.addisonbeck.dev";

  specials = [ special_nixos ];
  systems = [ system_dev ];
  users = [ user_me_at_dev ];

  all = systems ++ specials ++ users;
in
{
  "github.age".publicKeys = all;
}
