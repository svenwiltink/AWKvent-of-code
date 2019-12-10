# points contains all aseroids. The key of the array is x:y
BEGIN {
	FS=""
}

{
	for (i=1; i<=NF; i++) {
		if ($i == "#") {
			x = i-1
			y = NR -1
			if (debug) printf "found asteroid at X: %d, Y: %d\n", i-1, NR-1 
			points[x ":" y] = 1
		}
	}
}

# find the amount of asteroids viewable from coord
function findViewable(coord, points, 		asteroid) {
	count = 0
	for (asteroid in points) {
		if (asteroid == coord) {
			continue
		}
		if (!checkLOS(coord, asteroid, points)) {
			if (debug) printf "%s has LOS with %s\n", coord, asteroid
			count++
		}
	}

	return count	
}

# loop over all other points to check if they block the view.
# return true if another asteroid is blocking LOS
function checkLOS(coordA, coordB, points, 		asteroid) {
	split(coordA, coordsA, ":")
	split(coordB, coordsB, ":")

	aX = int(coordsA[1])
	aY = int(coordsA[2])

	bX = int(coordsB[1])
	bY = int(coordsB[2])

	return 0
}

END {
	max = 0
	for (coord in points) {
		count = findViewable(coord, points)
		if (count > max) {
			max = count
			maxPosition = coord
		}

		printf "%s can view %s\n", coord, count
	}

	print maxPosition
}