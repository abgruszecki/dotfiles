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

## How does it work?
You should have a git repo with configurations.
Doesn't need to be public.
In the repo there's one directory per a remote machine.
On the remote machine you symlink files from that dir where they should go
(consider using GNU stow).

There's a local and a remote repo.
(KISS: you must keep them both in the home, and not rename the repo directory.)
Locally you have the master branch checked out.
On the remote you have a special `dev` branch,
which should always be ahead of the remote copy of the master branch.
Locally, you also have a git remote for the remote repository.
This lets you use `git` to push and pull changes, while keeping things as simple as possible.

# Dev notes
Remember: one reason for piping scripts into `ssh bash -` is remotely executing local scripts.
You can develop the script locally and then exec it remotely, no need to push it first.

What is the "all clear" state?
`git -C dotfiles-private log`
All remotes have `master`, `dev` pointing to HEAD.

You need to push before you pull.
Pushing moves the remote master and rebases dev on top of it.
Then you can pull and you get a dev which you can re-integrate back into master.

At least I think pulling will make things reasonable.
As a sanity check, you can also run `git fetch` for all remotes:
`parallel -j1 --lb git -C dotfiles-private fetch {} ::: explorer boa robolang robolidar perlmutter`

Running commands on multiple hosts?
```text
$ tmux
$ tmux set -g remain-on-exit on
$ parallel tmux neww -n {} $CMD {} ::: explorer boa robolang robolidar perlmutter
```
KEEP THE COMMAND SIMPLE - - `parallel` uses Bash to interpret its template. 

Use `tmux respawn-pane` to restart commands in case something goes wrong, quite useful.
Use `tmux kill-window -a` to kill all other windows.

Remember to consider:

```bash
curl -sSfL https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | ssh explorer sh -
```

### Easily running commands on remote hosts
Ultimately it's probably easiest to just start many terminals, set variables, and copy-paste a command.

There was an entire small saga with using `parallel` to run a commandline on remote hosts.

#### Saga
The problem: we want parallel to start tmux windows.
So we need to go through _many_ layers before the command gets executed.
Some of them support interpreting the command with Bash, others don't.
In the end it's really easiest to just copy-paste the stupid command.

There's `parallel --pipe --tee` can help circumvent running inside tmux:
```bash
seq 1000 | parallel --pipe --tee -v wc {} ::: -w -l -c
```

Quoting is just an annoyance here:
```bash
parallel tmux neww -n{} ssh {} ln -srv '\~/dotfiles-private/{}/bash-local' '\~/.bash.d/local' ::: explorer boa robolang robolidar perlmutter
```
But here, despite the `echo`, `parallel` will run part of the command _locally_:
``` bash
parallel -j1 echo tmux neww -n{} ssh {} 'cd \~/dotfiles && stow -v git' ::: explorer boa robolang robolidar perlmutter
```

### TODO Write `rdot-do`
The command just runs a script on a remote via ssh.
Maybe with the prelude included?

### TODO Install shellcheck, vim config, maybe even OSH?
- https://oils.pub/osh.html
- https://mywiki.wooledge.org/BashFAQ/105
- https://www.shellcheck.net/
### TODO Reuse connections
Create a socket for the connections, why not?
We may as well handle this on the script level,
no need to rely on ssh being configured appropriately.
In either case rdot should start by interactively sequentially connecting to the servers
so that I have a chance to enter my password etc (e.g., to Aurora).

### TODO should this also install remote programs?
Like uv, neovim, fzf, zoxide, vd?

It's different functionality.
But it'd be good to have it somewhere anyway.

- vd is installed with `uv`: `uv tool install visidata`
