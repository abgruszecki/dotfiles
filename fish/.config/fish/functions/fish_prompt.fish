# Consider using https://github.com/pure-fish/pure or tide for inspiration
function _minimal_prompt_format_duration --argument-names duration_ms
    if test $duration_ms -lt 60000
        printf '%.1fs' (math --scale=1 "$duration_ms / 1000")
    else
        set -l total_seconds (math "$duration_ms / 1000")
        # Truncate fractions (round down)
        set -l minutes (math --scale=0 "$total_seconds / 60")
        set -l seconds (math --scale=0 "$total_seconds % 60")
        printf '%sm %ss' $minutes $seconds
    end
end

function fish_prompt
    set -l last_status $status
    set -l last_duration $CMD_DURATION

    printf '\n'
    set_color blue
    printf '%s' (prompt_pwd)
    set_color normal

    if test -n "$last_duration"; and test $last_duration -ge 5000
        printf ' %s' (_minimal_prompt_format_duration $last_duration)
    end

    if test $last_status -ne 0
        printf ' '
        set_color red
        printf '[%s]' $last_status
        set_color normal
    end

    printf '\n'

    if test $last_status -eq 0
        set_color green
    else
        set_color red
    end

    printf '❯ '
    set_color normal
end
