function ,py-venv
    set -l VENV_PATH ./.venv
    echo "Sourcing from: $VENV_PATH"
    if path is --type file $VENV_PATH/bin/activate.fish
        source $VENV_PATH/bin/activate.fish
	return 0
    end
    printf "Failed. "

    set -l VENV_PATH ./venv
    echo "Sourcing from: $VENV_PATH"
    if path is --type file $VENV_PATH/bin/activate.fish
        source $VENV_PATH/bin/activate.fish
	return 0
    end
    printf "Failed. "

    set -l VENV_PATH ./
    echo "Sourcing from: $VENV_PATH"
    if path is --type file $VENV_PATH/bin/activate.fish
        source $VENV_PATH/bin/activate.fish
	return 0
    end
    printf "Failed. "

    echo "Did not find a venv here."
    set -l VENV_PATH ./.venv
    while true
        read -l -P "Create a venv in $VENV_PATH ? [y/N] " confirm

        switch $confirm
            case Y y
		echo "Ok. Creating..."
                break
            case '' N n
		echo "Ok. Exiting..."
                return 0
        end
    end
    uv venv $VENV_PATH
    source $VENV_PATH/bin/activate.fish
end
