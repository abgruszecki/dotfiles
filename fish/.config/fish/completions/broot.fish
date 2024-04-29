complete -c broot -l conf -d 'Semicolon separated paths to specific config files' -r
complete -c broot -l outcmd -d 'Where to write the produced cmd (if any)' -r -F
complete -c broot -l verb-output -d 'Optional path for verbs using `:write_output`' -r -F
complete -c broot -s c -l cmd -d 'Semicolon separated commands to execute' -r
complete -c broot -l color -d 'Whether to have styles and colors' -r -f -a "{auto	'',yes	'',no	''}"
complete -c broot -l height -d 'Height (if you don\'t want to fill the screen or for file export)' -r
complete -c broot -l set-install-state -d 'Manually set installation state' -r -f -a "{undefined	'',refused	'',installed	''}"
complete -c broot -l print-shell-function -d 'Print to stdout the br function for a given shell' -r
complete -c broot -l listen -d 'A socket to listen to for commands' -r
complete -c broot -l write-default-conf -d 'Write default conf files in given directory' -r -F
complete -c broot -l send -d 'A socket to send commands to' -r
complete -c broot -l help -d 'Print help information'
complete -c broot -l version -d 'print the version'
complete -c broot -s d -l dates -d 'Show the last modified date of files and directories'
complete -c broot -s D -l no-dates -d 'Don\'t show the last modified date'
complete -c broot -s f -l only-folders -d 'Only show folders'
complete -c broot -s F -l no-only-folders -d 'Show folders and files alike'
complete -c broot -l show-root-fs -d 'Show filesystem info on top'
complete -c broot -s g -l show-git-info -d 'Show git statuses on files and stats on repo'
complete -c broot -s G -l no-show-git-info -d 'Don\'t show git statuses on files and stats on repo'
complete -c broot -l git-status -d 'Only show files having an interesting git status, including hidden ones'
complete -c broot -s h -l hidden -d 'Show hidden files'
complete -c broot -s H -l no-hidden -d 'Don\'t show hidden files'
complete -c broot -s i -l git-ignored -d 'Show git ignored files'
complete -c broot -s I -l no-git-ignored -d 'Don\'t show git ignored files'
complete -c broot -s p -l permissions -d 'Show permissions'
complete -c broot -s P -l no-permissions -d 'Don\'t show permissions'
complete -c broot -s s -l sizes -d 'Show the size of files and directories'
complete -c broot -s S -l no-sizes -d 'Don\'t show sizes'
complete -c broot -l sort-by-count -d 'Sort by count (only show one level of the tree)'
complete -c broot -l sort-by-date -d 'Sort by date (only show one level of the tree)'
complete -c broot -l sort-by-size -d 'Sort by size (only show one level of the tree)'
complete -c broot -l sort-by-type -d 'Same as sort-by-type-dirs-first'
complete -c broot -l no-tree -d 'Do not show the tree, even if allowed by sorting mode'
complete -c broot -l tree -d 'Show the tree, when allowed by sorting mode'
complete -c broot -l sort-by-type-dirs-first -d 'Sort by type, directories first (only show one level of the tree)'
complete -c broot -l sort-by-type-dirs-last -d 'Sort by type, directories last (only show one level of the tree)'
complete -c broot -l no-sort -d 'Don\'t sort'
complete -c broot -s w -l whale-spotting -d 'Sort by size, show ignored and hidden files'
complete -c broot -s t -l trim-root -d 'Trim the root too and don\'t show a scrollbar'
complete -c broot -s T -l no-trim-root -d 'Don\'t trim the root level, show a scrollbar'
complete -c broot -l install -d 'Install or reinstall the br shell function'
complete -c broot -l get-root -d 'Ask for the current root of the remote broot'
