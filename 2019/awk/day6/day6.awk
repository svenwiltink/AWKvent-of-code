BEGIN {
	FS=")"
}

{
	paths[$1] = paths[$1] == "" ? $2 : $2 "," paths[$1] 
	paths[$2] = paths[$2] == "" ? $1 : $1 "," paths[$2] 
}


function walkPath(node, depth, walked,		i, targets, orbits) {
	if (node in walked) {
		return 0
	}

	walked[node] = 1
	orbits = depth
	depth++

	split(paths[node], targets, ",")

	if (debug) {
		printf "Depth: %d\n", depth
		printf "Walking from %s. Depth %d\n", node, depth
		printf "Targets: %s\n", paths[node]
	}

	for (i in targets) {
		orbits += walkPath(targets[i], depth, walked)
	}

	return orbits
}

function findPath(node, target, walked, 	i, targets) {
	if (node == target) {
		print  "target reached"
		return 0
	}

	if (node in walked) {
		printf "node %s already walked\n", node
		return -1
	}

	if (debug) {
		printf "Walking from %s\n", node
		printf "Targets: %s\n", paths[node]
	}

	walked[node] = 1

	split(paths[node], targets, ",")

	for (i in targets) {
		if (debug) {
			printf "Trying target %s\n", targets[i]
		}

		l = findPath(targets[i], target, walked)
		if (l == -1) {
			if (debug) {
				print "dead end!"
			}
			continue
		}

		return l + 1
	}

	return -1
}

END {
	part1 = walkPath("COM", 0, walked1)
	part2 = findPath("YOU", "SAN", walked2) - 2

	printf "Part1: %s\n", part1
	printf "Part2: %s\n", part2
}