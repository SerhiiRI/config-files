#!/bin/bash
# Erdlöwe theme by DarthWound - run

crtbld=$(cat /usr/share/themes/Erdlowe/version.txt)
lstbld=$(wget -qO - 'https://github.com/DarthWound/erdlowe-theme/releases/latest' | grep -o 'download/v[0-9]\.[0-9]-[0-9]\{8\}' | tr -d '[download/]')

gtk="/org/gnome/desktop/interface/gtk-theme"
wm="/org/gnome/desktop/wm/preferences/theme"
gs="/org/gnome/shell/extensions/user-theme/name"
ico="/org/gnome/desktop/interface/icon-theme"
fnttitle="/org/gnome/desktop/wm/preferences/titlebar-font"
fntsans="/org/gnome/desktop/interface/font-name"
fntserif="/org/gnome/desktop/interface/document-font-name"
fntmono="/org/gnome/desktop/interface/monospace-font-name"
gdtschm="/org/gnome/gedit/preferences/editor/scheme"

installtheme() {
	printf "Installing theme...\n"
	sleep 2s
	cd /usr/share/themes
	sudo rm -rf Erdlowe*
	sleep 4s
	sudo wget https://github.com/DarthWound/erdlowe-theme/releases/download/$lstbld/erdlowe.tar.gz
	sudo tar -xzf erdlowe.tar.gz
	printf "Cleaning...\n"
	sleep 4s
	sudo rm erdlowe.tar.gz
	read -p "Theme installed! Apply it now? [Y/n]: " a
		if [ "$a" = "Y" ] || [ "$a" = "y" ]
			then
				dconf write $gtk "'Erdlowe'" && dconf write $wm "'Erdlowe'" && dconf write $gs "'Erdlowe-compact'"
			else
				printf "Ok." && sleep 2s
		fi
	read -p "Apply suggested fonts now? (only if Source Pro fonts are installed) [Y/n]: " a
		if [ "$a" = "Y" ] || [ "$a" = "y" ]
			then
				dconf write $fnttitle "'Source Sans Pro Bold 10'" && dconf write $fntsans "'Source Sans Pro 12'" && dconf write $fntserif "'Source Serif Pro 12'" && dconf write $fntmono "'Source Code Pro 12'"
			else
				printf "Ok." && sleep 2s
		fi
	read -p "Done! Open 'GNOME Tweaks' app to change themes, icons, and fonts, if you want. Press ENTER to close."
}

updatetheme() {
	printf "Updating theme...\n"
	sleep 2s
	dconf write $gtk "'Adwaita'" && dconf write $wm "'Adwaita'" && dconf write $gs "''"
	cd /usr/share/themes
	sudo rm -rf Erdlowe*
	sleep 4s
	sudo wget https://github.com/DarthWound/erdlowe-theme/releases/download/$lstbld/erdlowe.tar.gz
	sudo tar -xzf erdlowe.tar.gz
	printf "Cleaning...\n"
	sleep 4s
	sudo rm erdlowe.tar.gz
	sleep 2s
	dconf write $gtk "'Erdlowe'" && dconf write $wm "'Erdlowe'" && dconf write $gs "'Erdlowe-compact'"
	read -p "Done! Open 'GNOME Tweaks' app to change themes if you want. Press ENTER to close."
}

getrequired() {
	printf "Installing requirements...\n"
	sleep 2s
	sudo zypper ref
	sudo zypper in gtk2-engine-murrine adobe-source*pro-fonts gnome-tweak-tool dconf
	read -p "Done! Press ENTER to close."
}

getsuggested() {
	printf "Installing icons...\n"
	sleep 2s
	wget -qO- https://raw.githubusercontent.com/PapirusDevelopmentTeam/papirus-icon-theme/master/install.sh | sh
	sleep 2s
	wget -qO- https://raw.githubusercontent.com/PapirusDevelopmentTeam/papirus-folders/master/install.sh | sh
	sleep 2s
	papirus-folders -C grey
	dconf write $ico "'Papirus'"
	read -p "Done! Open 'GNOME Tweaks' app to change icons if you want. Press ENTER to close."
}

getextras() {
	printf "Installing extras...\n"
	sleep 2s
	wget -qO- https://raw.githubusercontent.com/DarthWound/erdlowe-theme/master/extras/erdlowe-terminal.sh | sh
	sudo wget -P /usr/share/gtksourceview-3.0/styles https://raw.githubusercontent.com/DarthWound/erdlowe-theme/master/extras/erdlowe-gedit.xml
	sleep 2s
	dconf write $gdtschm "'erdlowe'"
	read -p "Done! Press ENTER to close."
}

explorerepo() {
	printf "Opening web browser...\n"
	sleep 2s
	xdg-open https://github.com/DarthWound/erdlowe-theme
}

nukem() {
	printf "Cleaning theme, icons, and reverting to defaults...\n"
	sleep 2s
	sudo rm -rf /usr/share/themes/Erdlowe*
	sudo rm -rf /usr/share/icons/*Papirus*
	dconf write $gtk "'Adwaita'" && dconf write $wm "'Adwaita'" && dconf write $gs "''"
	dconf write $fnttitle "'Sans Bold 10'" && dconf write $fntsans "'Sans 12'" && dconf write $fntserif "'Serif 12'" && dconf write $fntmono "'Monospace 12'"
	dconf write $ico "'Adwaita'"
	dconf write $gdtschm "'classic'"
	read -p "Done! Open Terminal settings to change its theme if you installed extras. Press ENTER to close."
}

changelog() {
	printf "Opening web browser...\n"
	sleep 2s
	xdg-open https://github.com/DarthWound/erdlowe-theme/releases/latest
}

clear
printf "Launching Erdlowe installation script..."
sleep 2s
while [ 1 ];do
clear
cat << "EOF"
 ______         _ _ _   _             
|  ____|       | | (_) (_)            
| |__   _ __ __| | | _____      _____ 
|  __| | '__/ _` | |/ _ \ \ /\ / / _ \
| |____| | | (_| | | (_) \ V  V /  __/
|______|_|  \__,_|_|\___/ \_/\_/ \___|

                   a material openSUSE+GNOME theme                                               
EOF
if [ "$crtbld" != "$lstbld" ]
	then
		printf "\n\n\n   BUILD $crtbld\n   - BUILD $lstbld available!\n   - Type \"0\" to read changelog."
	else
		printf "\n\n\n   BUILD $crtbld"
fi
printf "\n
   \e[1m1\e[0m => \e[1mInstall\e[0m (check dependencies before)
   \e[1m2\e[0m => \e[1mUpdate\e[0m (if Erdlowe is already installed)
   \e[1m3\e[0m => \e[1mGet required dependencies\e[0m (openSUSE only)
   \e[1m4\e[0m => \e[1mGet suggested icons\e[0m (Papirus grey)
   \e[1m5\e[0m => \e[1mGet extras\e[0m (Gedit and Terminal themes)
   \e[1m6\e[0m => \e[1mExplore GitHub repository\e[0m
   \e[1m7\e[0m => \e[1mRemove everything\e[0m
   \e[1m8\e[0m => \e[1mQuit\e[0m
\n"
	printf "\n   \e[1mTYPE NUMBER OF SELECTED OPTION THEN PRESS ENTER:\e[0m "
	read _slct
	case $_slct in
		0)
			clear
			changelog
			sleep 2s
		;;
		1)
			clear
			installtheme
			sleep 2s
		;;
		2)
			clear
			updatetheme
			sleep 2s
		;;
		3)
			clear
			getrequired
			sleep 2s
		;;
		4)
			clear
			getsuggested
			sleep 2s
		;;
		5)
			clear
			getextras
			sleep 2s
		;;
		6)
			clear
			explorerepo
			sleep 2s
		;;
		7)
			clear
			nukem
			sleep 2s
		;;
		8)
			clear
			printf "Exiting script..."
			sleep 2s
			clear
			exit 0
			break
		;;
	esac
done
