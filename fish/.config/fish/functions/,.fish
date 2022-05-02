function __comma_locate
    if test -d ,
        echo "$PWD/,"
        return 0
    end
    if test $PWD = "/"
        return 0
    end
    pushd ..
    __comma_locate
    popd
end

function __comma_print_methods
    set -l comma_dir (__comma_locate)
    if test "$comma_dir" = ""
        echo >&2 "Not under a comma dir."
        return 1
    end

    find $comma_dir -type f -executable -printf '%f\n'
end

function __comma_no_command
    set -l tok_ln (commandline -p -o)
    set -l ln (commandline -p)
    # No command if second arg is empty, or if we're writing the second arg
    # (i.e. there's no third arg and entire commandline doesn't end in space)
    test -z $tok_ln[2] || test -z $tok_ln[3] && string match -r '.*[^ ]$' $ln > /dev/null
end

function ,
    set -l comma_dir (__comma_locate)
    if test "$comma_dir" = ""
        echo >&2 "Not under a comma dir."
        return 1
    end

    if test "$argv[1]" = ""
        echo >&2 "Missing method argument. Usage: , <METHOD>"
        return 1
    end

    "$comma_dir/$argv[1]" $argv[2..-1]
end

complete -c , -e
complete -c , -x -n "__comma_no_command" -d "Method" -a "(__comma_print_methods)"
# complete -c , -d "Method" -a "(__comma_print_methods)"
