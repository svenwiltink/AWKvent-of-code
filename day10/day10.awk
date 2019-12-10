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
			current = viewMap[coords]

			if (current == "") {
				viewMap[coords] = asteroid
			} else {
				viewMap[coords] = viewMap[coords] "," asteroid
			}

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

	if (debug) printf "checking LOS for x: %d, y: %d and x: %d, y: %d\n", aX, aY, bX, bY

	vertical = 0
	a = 0
	b = 0
	# special case. This line is vertical. Only do bounds check for other points
	if (bX == aX) {
		if (debug) printf "found a special case. line between %s and %s is vertical\n", coordA, coordB
		vertical = 1
	} else {
		a = (bY - aY) / (bX - aX)
		b = aY - (a * aX)

		if (debug) printf "formula: y = %fx + %f\n", a, b
	}

	if (debug) printf "X: %f, Y: %f\n", aX, aY
 
 	xMin = aX < bX ? aX : bX
 	xMax = aX > bX ? aX : bX

 	yMin = aY < bY ? aY : bY
 	yMax = aY > bY ? aY : bY

	for (asteroid in points) {
		if (asteroid == coordA || asteroid == coordB) {
			continue
		} 

		split(asteroid, coordsC, ":")
		cX = coordsC[1]
		cY = coordsC[2]

		# check special vertical case. Only a bounds check is needed
		if (vertical){
			if (cX == aX && cY > yMin && cY < yMax) {
				if (debug) printf "X: %s, Y: %s is blocking LOS on a vertical line\n", cX, cY
				return 1
			} else {
				continue
			}
		}

		# check if the asteroid is within the 'box' around A and B
		if (cX < xMin || cX > xMax || cY < yMin || cY > yMax) {
			if (debug) printf "%s is out of bounds\n", asteroid
			continue
		}

		aC = (cY - aY) / (cX - aX)
		bC = cY - (a * aC)

		if (aC == a && bC == b) {
			if (debug) printf "sup bitches, we an asteroid has blocked LOS: %s\n", asteroid
			return 1
		}
	}

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