#!/bin/bash

current_pane="$(tmux display-message -p '#D')"

command="$1 ; sleep $2"

tmux split-window -t $current_pane "$command"

pid="$(tmux display-message -p '#{pane_pid}')"
tmux select-pane -t $current_pane # because of the lack of `-d` flag used in `split-window`, we should switch back.
sleep $2

kill -9 $pid
