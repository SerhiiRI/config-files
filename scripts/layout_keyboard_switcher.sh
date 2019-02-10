#!/bin/bash

# LICENSE: PUBLIC DOMAIN
# switch between my layouts

# If an explicit layout is provided as an argument, use it. Otherwise, select the next layout from
# the set [us, pl, ua].
if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | grep '^layout:' |awk '{print $2}')
    case $layout in
        us)
                setxkbmap pl
            ;;
        pl)
                setxkbmap ua
            ;;
        *)
                setxkbmap us
            ;;
    esac
fi
