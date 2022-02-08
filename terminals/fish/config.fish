# set -gx PATH $HOME/.local/bin/:$HOME/.cargo/bin/:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin $PATH

# function fish_greeting
# 	 echo "            +-----------------+"
# 	 echo "            | Kwa-kwa         |"
# 	 echo "            |    Motherkwaka! |"
# 	 echo "            +-| /-------------+"
# 	 echo "         __   |/"
# 	 echo "     ___( o)>"
# 	 echo "     \\ <_. )"
# 	 echo "   ~~~`~~-'~~~"
# 	 echo ""
# end

function fish_greeting
	 echo -e "            +-----------------+"
	 echo -e "            | Peace was never |"
	 echo -e "            |      an option. |"
	 echo -e "            +-| /-------------+"
	 echo -e "          __  |/         "
	 echo -e "      ___( o)\033[0;33m>\033[0m         "
	 echo -e "      \ <_. )    \033[0;32m \033[0m     "
	 echo -e "  \033[0;34m~~~~~\033[0m`---'\033[0;34m~~~~~\033[0m"
	 echo -e ""
end

#---------#
# ALIACES #
#---------#

alias emdit="emacsclient -c -a emacs"
alias emdit-nw="emacsclient -nw -c -a emacs"
alias emcalc="emacs -f full-calc"
alias emcalc-nw="emacs -nw -f full-calc"


#--------------#
# KEYBOARD MAP #
#--------------#

function keyboard-no-caps
	 setxkbmap -option "ctrl:nocaps"
end
function keyboard-cursor-rate-200-100
	 xset r rate 200 100
end
function keyboard-cursor-rate-180-120
	 xset r rate 180 120
end
function keyboard-preset-default
	 keyboard-no-caps
	 keyboard-cursor-rate-200-100
end
function keyboard-preset-warp
	 keyboard-no-caps
	 keyboard-cursor-rate-180-120
end
function update_nitrogen
	 nitrogen --restore
end


#-------------------#
# USEFULL FUNCTIONS #
#-------------------#

function fix-ntfs
	sudo ntfsfix /dev/nvme0n1p2
	sudo ntfsfix /dev/nvme0n1p4
end


#-----------------#
# MONITOR PRESETS #
#-----------------#

function mon-hdmi-hd-right-of
	 xrandr --output HDMI1 --mode 1920x1080 --right-of eDP1
end

function mon-hdmi-hd-left-of
	 xrandr --output HDMI1 --mode 1920x1080 --left-of eDP1
end

function mon-dell2mon
	 xrandr --output VGA1 --auto
	 xrandr --output VGA1 --right-of LVDS1 --output LVDS1 --primary
end

function mon-lgdell
	 xrandr --output LVDS1 --off
	 xrandr --output HDMI1 --mode 1920x1080
	 xrandr --output VGA1 --auto
	 xrandr --output VGA1 --auto --right-of HDMI1
end

function mon-onemonitor
	 xrandr --output HDMI1 --off
	 xrandr --output HDMI2 --off
	 xrandr --output DP1 --off
	 xrandr --output DP2 --off
	 xrandr --output eDP1 --auto
end

