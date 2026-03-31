#!/usr/bin/env bash
_tmux_broadcast_keys () {
    for _pane_id in $(tmux list-panes -s -F '#{pane_id}'); do
        tmux send-keys -t "${_pane_id}" "$@"
    done
}

while IFS= read -r line; do
    _tmux_broadcast_keys -l "$line" $'\n'
done < <(vipe)
