#!/bin/bash
find $HOME/.wallpaper -name '*.jpg' -o -name '*.png' | shuf -n 3 | xargs feh --bg-scale
