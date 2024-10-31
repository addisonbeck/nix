{pkgs, ...}: {
  home.packages = [
    (pkgs.writeShellScriptBin "toggle-sleep-osx" ''
	sleepvar="$(pmset -g | grep SleepDisabled)"
	sleepval="$(grep -c 0 <<< $sleepvar)"
	if [ "$sleepval" -eq 1 ];
	    then 
		sudo pmset disablesleep 1
		echo "Disabling sleep"
	    else
		sudo pmset disablesleep 0
		echo "Enabling sleep"
	fi
    '')
  ];
}
