{ pkgs, ... }: {
  home.packages = [
    (pkgs.writeShellScriptBin "nuke-docker" ''
      	docker stop $(docker ps -a -q)
      	docker rm $(docker ps -a -q)
      	docker volume rm $(docker volume ls -q)
              docker network prune
    '')
  ];
}
