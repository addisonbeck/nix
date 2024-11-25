{pkgs ? import <nixpkgs> {}, ...}: let
  # configContent = ''
  #   host = "localhost"
  #   port = 3306
  #   user = "user"
  #   password = "password"
  #   database = "dbname"
  #
  #   [[queries]]
  #   name = "query1"
  #   sql = "SELECT * FROM table1"
  #
  #   [[queries]]
  #   name = "query2"
  #   sql = "SELECT * FROM table2 WHERE column1 = "value""
  #
  #   [logging]
  #   level = "INFO"
  #   file = "lazysql.log"
  #
  #   [settings]
  #   timeout = 30
  #   retries = 3
  # '';
in {
  home.packages = [pkgs.lazysql];
  # home.file.".config/lazysql/config.toml".text = configContent;
}
