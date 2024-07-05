function ,venv
    set -l VENV_PATH ./.venv
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
    while true
        read -l -P 'Create a venv in ./.venv ? [y/N] ' confirm

        switch $confirm
            case Y y
		echo "Ok. Creating..."
                break
            case '' N n
		echo "Ok. Exiting..."
                return 0
        end
    end
    python3 -m venv ./.venv
    source ./.venv/bin/activate.fish
end
