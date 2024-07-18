# Taken from https://raw.githubusercontent.com/sigoden/aichat/main/scripts/shell-integration/integration.fish

function _aichat_fish
    set -l _old (commandline)
    if test -n $_old
        echo -n "⌛"
        commandline -f repaint
        commandline (aichat -e $_old)
    end
end
