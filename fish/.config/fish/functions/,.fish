function __comma_locate
    if test -d ,
        echo "$PWD/,"
        return 0
    end
    if test $PWD = "/"
        return 0
    end
    # Use recursion to traverse the parents.
    # Not like this can run out of stack...
    pushd ..
    __comma_locate
    popd
end

function __comma_print_methods
    set -l comma_dir (__comma_locate)
    if test "$comma_dir" = ""
        printf >&2 '\nNot under a comma dir.\n'
        return 1
    end

    find -L $comma_dir -type f -executable -printf '%f\n'
end

function __comma_in_2nd_arg
    # Use `set -g` to debug these.
    set -l tok_ln (commandline -p -o)
    set -l ln (commandline -p)
    # No command if second arg is empty, or if we're writing the second arg
    # (i.e. there's no third arg and entire commandline doesn't end in space)
    test -z $tok_ln[2]
    or test -z $tok_ln[3] && string match -r '.*[^ ]$' $ln > /dev/null
end

function __comma_subcommand_expects_method
    # See above.
    set -g __called __comma_subcommand_expects_method
    set -g tok_ln (commandline -p -o)
    set -g ln (commandline -p)

    test "$tok_ln[2]" = ':cat'
    or test "$tok_ln[2]" = ':path'
    or return 1

    # Are we in *3rd* arg?
    test -z "$tok_ln[3]"
    or test -z "$tok_[4]" && string match -r '.*[^ ]$' $ln > /dev/null
end

function ,
    set -l comma_dir (__comma_locate)
    if test "$comma_dir" = ""
        echo >&2 "Not under a comma dir."
        return 1
    end

    if test "$argv[1]" = ""
        echo >&2 "Missing command or method argument. Usage: , <COMMAND OR METHOD>"
        return 1
    end

    if test "$argv[1]" = ":dir"
        echo "$comma_dir"
        return
    end

    if test "$argv[1]" = ":cat"
        if test -z "$argv[2]"
            echo >&2 "Command expected a method argument. Usage: , :cat <METHOD>"
            return 1
        end
        cat "$comma_dir/$argv[2]"
        return
    end

    if test "$argv[1]" = ":path"
        if test -z "$argv[2]"
            echo >&2 "Command expected a method argument. Usage: , :path <METHOD>"
            return 1
        end
        echo "$comma_dir/$argv[2]"
        return
    end

    "$comma_dir/$argv[1]" $argv[2..-1]
end

complete , -e
complete , -x -n "__comma_in_2nd_arg" -d "Method" -a "(__comma_print_methods)"
complete , -x -n "__comma_in_2nd_arg" -d "Command" -a ":dir :cat :path"
complete , -x -n "__comma_subcommand_expects_method" -d "Method" -a "(__comma_print_methods)"
# complete -c , -d "Method" -a "(__comma_print_methods)"
