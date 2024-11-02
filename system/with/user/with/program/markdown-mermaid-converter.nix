{pkgs, ...}: {
  home.packages = [
    (pkgs.writeShellScriptBin "markdown-mermaid-converter" ''
      mmdc -i $1.md -o $1.generated.md
    '')
  ];
}
