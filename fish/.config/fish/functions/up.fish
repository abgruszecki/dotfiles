function up -a dir
    if [ -z "$dir" ]
        return
    end
    set target (string replace -r "/\Q$dir\E/.*" "/$dir" $PWD)
    echo $target
    cd $target
end

function __up_complete
    set -l path_elts (string split -n / $PWD)
    for i in (seq (count $path_elts) -1 1)
        echo $path_elts[$i]
    end
end

complete -x -k -c up -a '(__up_complete)'
