#!/usr/bin/env fish

function ,py-pip-install-save
    set pkg $argv[1]
    set tgt $argv[2]
    if not test "$tgt"
        set tgt 'requirements.txt'
    end
    pip install $pkg
    or return
    if test '-' = "$tgt"
        pip freeze | grep $pkg
    else
        pip freeze | grep $pkg >> $tgt
    end
end
