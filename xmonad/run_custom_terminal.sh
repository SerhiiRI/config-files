#!/bin/bash

hour=$(date +%H)
if [ "$hour" -lt 17 -a "$hour" -ge 8 ]; then
   gnome-terminal --profile 'bright'
else
   gnome-terminal --profile 'dracula'
fi
