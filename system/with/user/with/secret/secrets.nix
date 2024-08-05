let
  me_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH";
in
{
  "github.age".publicKeys = [ me_key ];
}
