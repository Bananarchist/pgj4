#!/bin/sh

# declarations
output_dir="dist"
src_dir="src"
elm_dir="$src_dir/elm"
html_dir="$src_dir/html"
js_dir="$src_dir/js"
css_dir="$src_dir/css"
debug=false
dist=false

for arg in $@; do
	if [[ $arg =~ "debug" ]]; then
		debug=true
	elif [[ $arg =~ "dist" ]]; then
		dist=true
#	elif [[ $arg =~ "editor" ]]; then
#		entry_dir="src/Editor"
#		output_dir="dist/editor"
 	else
		echo "Unexpected argument $arg"
	fi
done

# create output directories
mkdir -p $output_dir
# mkdir -p $output_dir/assets

js_out="$output_dir/main.js"
entry="$elm_dir/Main.elm"
# compile elm code
if $debug; then
	echo "Building for debug"
	elm make $entry --output=$js_out --debug
else
	echo "Building for prod"
	elm make $entry --output=$js_out
fi

# copy resources to dist
cp $html_dir/index.html $output_dir
cp $css_dir/index.css $output_dir
cp $js_dir/websocket.js $output_dir
cp $js_dir/localstorage.js $output_dir

if $dist; then
	echo "Compressing client for distribution"
	zip -r client.zip $output_dir
fi
