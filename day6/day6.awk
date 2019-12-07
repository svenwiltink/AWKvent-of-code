BEGIN {
	FS=")"
}

{
	paths[$1] = paths[$1] == "" ? $2 : $2 "," paths[$1] 
}


function walkPath(node, depth, 		i, targets, orbits) {
	orbits = depth
	depth++

	split(paths[node], targets, ",")

	if (debug) {
		printf "Depth: %d\n", depth
		printf "Walking from %s. Depth %d\n", node, depth
		printf "Targets: %s\n", paths[node]
	}

	for (i in targets) {
		orbits += walkPath(targets[i], depth)
	}

	return orbits
}

END {
	print walkPath("COM", 0)
}