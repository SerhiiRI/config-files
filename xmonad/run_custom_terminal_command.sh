#!/bin/bash

echo "Title $1"
echo "Command $1"

hour=$(date +%H)
if [ "$hour" -lt 17 -a "$hour" -ge 8 ]; then
   gnome-terminal --title $1 --profile 'bright' --command $2
else
   gnome-terminal --title $1 --profile 'dracula' --command $2
fi
