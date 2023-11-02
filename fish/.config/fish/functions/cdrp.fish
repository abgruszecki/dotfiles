function cdrp
    if test -z "$argv[1]"
        cd (realpath .)
    else
        cd (realpath "$argv[1]")
    end
end
	
