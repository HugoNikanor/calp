#!/usr/bin/env bash

# Start a webdav server, and check if it behaves with some weird
# files.

root=$(dirname "$(dirname "$(realpath "$0")")")
eval "$(env __PRINT_ENVIRONMENT=1 "${root}/calp")"

tmpdir=$(mktemp --directory)
export tmpdir

config_file=$(mktemp)

cat - > "$config_file" <<-"EOF"
(use-modules (calp config-base))

((@ (calp entry-points webdav) webdav-resources)
  `(("/files" file
      path: ,(getenv "tmpdir"))
    ("/virtual" virtual
      content: ,((@ (ice-9 iconv) string->bytevector)
                    "Hello, World\n"
                    "ascii"))
   )
)
EOF

# configure weird files
echo 'File contents' > "$tmpdir/file"
if [ "$(id -u)" = 0 ]; then
    mknod char c 0 0
    mknod block b 0 0
fi
mkdir "$tmpdir/directory"
ln -s file "$tmpdir/working-symlink"
ln -s nonexistance "$tmpdir/broken-symlink"
mkfifo "$tmpdir/fifo"
"${root}/scripts/mksock" "$tmpdir/socket"

portpipe=$(mktemp --dry-run)
mkfifo "$portpipe"
"$root/calp" webdav --config "$config_file" --randport "$portpipe" &

port=$(cat "$portpipe")

curl -X PROPFIND \
     -H 'Depth: Infinity' \
     --silent \
     "http://localhost:$port" \
    | xmllint --format - \
    | highlight -S xml

rm "$config_file"
rm "$portpipe"
# rm -r "$tmpdir"
# TODO clean up test directory
