#!/usr/bin/env bash

declare -a options=("emacs"
		    "firefox"
		    "vivaldi-stable"
		    # "google-chrome-stable"
		    "spotify"
		    "deadbeef"
		    "inkscape"
		    "gimp"
		    # "nitrogen"
		    "qbittorrent"
		    "datagrip"
		    # "visualvm"
		    "slack"
		    "telegram-desktop"
		    "simplescreenrecorder"
		    "gnome-disks"
		    "gnome-calculator"
		    "gnome-system-monitor"
		    "selectdefaultapplication"
		    "lxappearance"
		    # "rawtherapee"
		    # "DadroitJSONViewer.AppImage"
		    # "dpscreenocr"
		    "font-manager"
		    # "kleopatra")
		    )

# Piping the above array (cleaned) into dmenu.
# We use "printf '%s\n'" to format the array one item to a line.
choice=$(printf '%s\n' "${options[@]}" | xwinmosaic -Vpr )

if [[ "$choice" == "quit" ]]; then
    echo "Quit. Program terminated." && exit 1
elif [ "$choice" ]; then
    cfg=$(printf '%s\n' "${choice}" | awk '{print $NF}')
    # shellcheck disable=SC2154
    exec "${cfg}"
    # What to do if we just escape without choosing anything.
else
    echo "Program terminated." && exit 0
fi
