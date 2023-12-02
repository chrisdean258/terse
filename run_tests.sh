#!/bin/bash


cargo build || exit 1

# 1 red
# 2 green
# 3 yellow
# 4 blue
# 5 magenta
# 6 cyan
# 7 white


run-test()
{
	rv="0"
	if ! diff -y <(./target/debug/terse "$1" 2> stderr.txt) "$1.out"; then
		rv=$((rv+1))
	fi
	if [ -n "$(cat "stderr.txt")" ]; then 
		rv=$((rv+2))
	fi
	rm "stderr.txt"
	return $rv
}

color() {
	tput setaf "$1"
	shift
	echo "$@"
	tput sgr0
}

if [ $# -eq 0 ]; then
	num_tests=0
	num_passed=0

	for file in ./tests/*.trs; do
		num_tests=$(($num_tests + 1))
		run-test "$file" &>/dev/null
		rv=$?
		if [ "$rv" -eq 0 ]; then
			num_passed="$((num_passed + 1))"
			color 2 "Passed $file"
		elif [ "$rv" -eq 1 ]; then
			color 5 "Failed $file"
		elif [ "$rv" -eq 2 ]; then
			color 3 "Failed $file"
		else
			color 1 "Failed $file"
		fi
	done
	echo "Passed $num_passed/$num_tests"
else 
	for t in "$@"; do 
		file="$(find ./tests -name '*.trs' | grep "$t" | head -n 1)"
		run-test "$file"
	done
fi


