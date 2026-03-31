would be nice to print job names but unfortunately can't really do that
anyway once this is polished the jobs should run in parallel
so the only option will be to have a job log and write output to files
(or to run the jobs in tmux windows? using the parallel option?)

"Pull" from a remote repo:
``` bash
rdot-repo-push.sh $repo $remote
rdot-repo-pull.sh $repo $remote
```
Currently to pull correctly it's necessary to "push" local changes first.
(The remote `dev` head must be rebased on top of the local `master`?)
IIRC if the repo isn't pushed first, later operations could potentially fail, or maybe the pull itself may fail?
(Maybe the "pull" actually pulls to the local `master`?)
(Thought: current pull should be semi-pull and new pull should both push and semi-pull.)
(Reflection: the need for the push is to resolve conflicts remotely.
Resolving them on the "central" machine would require pushing the resolution;
I didn't yet consider how to safely push the resolution.
Would committing changes remotely, fetching them, then pushing a merge commit work?
Maybe, but the changes should be tested on the remote machine.)

Currently the simplest way to run arbitrary commands on remote hosts:
```bash
remote=boa
tmux neww -e remote=$remote -n $remote
```
Option: use parallel to spawn many windows at once. Consider:
```bash
parallel --jl >(sponge /dev/stderr) -j1 --lb -- 
```
Also consider using `,tmux-broadcast.sh` to broadcast commented-out commands to all panes.
(
Distinguish remote panes by starting their names with `r:`?
Make all panes remain on exit?
Check for errors and crash panes if there are any?
(`ssh` seems to exit with the status of the remote command)
)
FUN idea: when an error occurs, send the bell character to the terminal:
```bash
tmux send-keys -t 2.1 -l $'\a'
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

## Running commands in parallel
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

### Waiting for the commands
First, remain-on-exit can be set in the commandline, since tmux interprets the commands with the shell.
Second, `tmux list-panes -s -F 'PANE_ID,#{pane_dead},#{pane_dead_status}'` can be used to wait for all the panes.
(Don't confuse the main pane with the "background" ones.)

### Complex commandlines
Ultimately it's probably easiest to just start many terminals, set variables, and copy-paste a command.

I went through an entire small saga with using `parallel` to run a commandline on remote hosts.

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


## Improving ssh scripts
Goal:
1. run the same scripts locally and via ssh
2. have a command which can run an "ssh script" file (which can source scripts)

Approach:
1. Write the main script with special comments for sourced scripts
2. -
   1. When running remotely, replace the comments with sources
   2. When running locally, replace the comments with source commands

## TODO Write `rdot-do`
The command just runs a script on a remote via ssh.
Maybe with the prelude included?

"Automatically" including the prelude is ugly but maybe the best out of bad choices.
The script can't source local files.
The script can be "templated" but that makes rdot-do behave strangely.

Seriously I just want to have `aloud` and `log` and `die` in the template script,
it's not that complicated.

## TODO Install shellcheck, vim config, maybe even OSH?
- https://oils.pub/osh.html
- https://mywiki.wooledge.org/BashFAQ/105
- https://www.shellcheck.net/
## TODO Let git take advantage of controlsocks
Right now we set controlsocks options via the commandline.
This doesn't work for ssh connections made through git,
and we make a lot of those.
So should the scripts expect controlsocks to exist?
Alternatively, can use `GIT_SSH_COMMAND`.

## TODO should `rdot` also install remote programs?
Like uv, neovim, fzf, zoxide, vd?

It's different functionality.
But it'd be good to have it somewhere anyway.

- vd is installed with `uv`: `uv tool install visidata`
