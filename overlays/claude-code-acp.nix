final: prev: {
  claude-code-acp = prev.claude-code-acp.overrideAttrs (oldAttrs: rec {
    version = "0.13.1";
    src = prev.fetchFromGitHub {
      owner = "zed-industries";
      repo = "claude-code-acp";
      rev = "v${version}";
      hash = "sha256-pG4iO2jVuzrSg7UfmOd2/gxsRKXLni8AWzpFEqtfu1s=";
    };
    npmDeps = prev.fetchNpmDeps {
      inherit src;
      hash = "sha256-AmQCjPBcTX+0HRpow8B+nBQ/uqrti6QmWqErX2FI9+Y=";
    };
  });
}
