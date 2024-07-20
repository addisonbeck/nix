let
  vm_system_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICQAykFwtp6bqQs69/EJfhrjGhva8JrIIrxlFL5kjS0P root@nixos";
  me_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHIpqhSpVDAEIXYcguordLgbt932yQ4RnyKDFhxpv9i6 me";
in
{
  "me.age".publicKeys = [ vm_system_key ];
  "github.age".publicKeys = [ me_key ];
}
