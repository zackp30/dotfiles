#!/bin/bash
find $HOME/.wallpaper -name '*.jpg' -o -name '*.png' | shuf -n 1 | xargs feh --bg-scale
