let
  pkgs = import <nixpkgs> {};
  deps = [
    pkgs.editorconfig-core-c
    pkgs.git
    pkgs.exa
    pkgs.devd
    pkgs.yarn
  ];

in
  pkgs.mkShell
    {
      buildInputs = deps;
      shellHook = ''
        set -o vi
        local pink='\e[1;35m'
        local yellow='\e[1;33m'
        local blue='\e[1;36m'
        local white='\e[0;37m'
        local reset='\e[0m'

        git_branch() {
          git rev-parse --abbrev-ref HEAD 2>/dev/null
        }

        export PS1="\[$pink\]nix \[$blue\]\W \[$yellow\]\$(git_branch)\[$white\] ∙ \[$reset\]"

        alias ls=exa
      '';
    }
