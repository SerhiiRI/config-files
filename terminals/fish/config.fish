function fish_greeting
	 echo "            +-----------------+"
	 echo "            | Kwa-kwa         |"
	 echo "            |    Motherkwaka! |"
	 echo "            +-| /-------------+"
	 echo "         __   |/"
	 echo "     ___( o)>"
	 echo "     \\ <_. )"
	 echo "   ~~~`~~-'~~~"
	 echo ""
end

function hdmi-hd-right-of
	 xrandr --output HDMI1 --mode 1920x1080 --right-of eDP1
end

function hdmi-hd-left-of
	 xrandr --output HDMI1 --mode 1920x1080 --left-of eDP1
end

function no-caps
	 setxkbmap -option "ctrl:nocaps"
end

function onemonitor
	 xrandr --output HDMI1 --off
	 xrandr --output HDMI2 --off
	 xrandr --output DP1 --off
	 xrandr --output DP2 --off
	 xrandr --output eDP1 --auto
end



