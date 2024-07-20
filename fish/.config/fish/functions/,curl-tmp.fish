function ,curl-tmp
    set -l url $argv[1]
    set -l filename (basename $url)
    set tmpdir /tmp/abg-curl-tmp
    mkdir -p $tmpdir
    set -g tmp "$tmpdir/$(date +'%Y%m%dT%H%M%S')-$filename"
    echo "Fetching to \$tmp: $tmp"
    curl -sSfL $url > $tmp
end
