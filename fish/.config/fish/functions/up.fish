function up -a dir
    if [ -z "$dir" ]
        return
    end
    set target (string replace -r "/\Q$dir\E/.*" "/$dir" $PWD)
    echo $target
    cd $target
end

complete -x -k -c up -a '(string split -n / $PWD)'
