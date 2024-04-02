let
  nixos_pub = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIITJNv1ewE1vKWYdXkpCmuQqvc0js217YB36FZq9qPMs nixos-dev-deployment-key";
in
{
  "github.age".publicKeys = [ nixos_pub ];
}
