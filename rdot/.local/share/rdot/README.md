would be nice to print job names but unfortunately can't really do that
anyway once this is polished the jobs should run in parallel
so the only option will be to have a job log and write output to files
(or to run the jobs in tmux windows? using the parallel option?)

"Pull" from a remote repo:
``` bash
rdot-repo-pull.sh $repo $remote
```

```bash
# fish code
# set repo dotfiles-private
# set rmt robolang
# ssh $rmt git -C "~/$repo" add --all
# ssh $rmt git -C "~/$repo" commit -m "$rmt"

git fetch robolang

git merge --ff-only robolang/dev
```

```bash
parallel --halt-on-error=2 --lb -j1 rdot-repo-push.sh dotfiles {} ::: explorer boa robolang robolidar
```

# "Documentation"
## What is this stuff?
Scripts for maintaining remote dotfiles/configurations.

Put your remote's configuration in files, track them in a repo,
keep a local copy of all the remote configurations so that
you can easily compare them and copy-paste between them,
easily push local changes to all the remote repos.

### TODO Reuse connections
Create a socket for the connections, why not?
We may as well handle this on the script level,
no need to rely on ssh being configured appropriately.
In either case rdot should start by interactively sequentially connecting to the servers
so that I have a chance to enter my password etc (e.g., to Aurora).

### TODO should this also install remote programs?
Like uv, neovim, fzf, zoxide?

It's different functionality.
But it'd be good to have it somewhere anyway.

## How does it work?
You should have a git repo with configurations.
Doesn't need to be public.
In the repo there's one directory per a remote machine.
On the remote machine you symlink files from that dir where they should go
(consider using GNU stow).

There's a local and a remote repo.
(For simplicity, you must keep them both in your home directory, and name them the same.)
Locally you have the master branch checked out.
On the remote you have a special `dev` branch,
which should always be ahead of the remote copy of the master branch.
Locally, you also have a git remote for the remote repository.
This lets you use `git` to push and pull changes, while keeping things as simple as possible.
