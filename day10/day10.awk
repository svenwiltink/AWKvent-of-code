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

	angleAB = atan2(bY-aY, bX-aX)
	distanceAB = (bY-aY)*(bY-aY) + (bX-aX)*(bX-aX) 

	if (debug) printf "AB angle %s, distance %s\n", angleAB, distanceAB

	for (asteroid in points) {
		if (asteroid == coordA || asteroid == coordB) {
			if (debug) printf "not checking same coord: %s\n", asteroid
			continue
		}

		split(asteroid, coordsC, ":")

		cX = int(coordsC[1])
		cY = int(coordsC[2])

		angleAC = atan2(cY-aY, cX-aX)
		distanceAC = (cY-aY)*(cY-aY) + (cX-aX)*(cX-aX) 
		if (debug) printf "AC angle %s, distance %s\n", angleAB, distanceAB

		if (angleAB == angleAC && distanceAC < distanceAB) {
			if (debug) print "WE HAVE A WINNER ", angleAB, angleAC 
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

	printf "postion %s, can observe %s\n", maxPosition, max
}

function calculateAngles(result, coord, points) {
	split(coord, coordsA, ":")

	aX = int(coordsA[1])
	aY = int(coordsA[2])


	SORTPOINT = coord
	amount = asorti(points, result, "compareDistanceAndAngle")
	for (i=1; i<= amount; i++) {
		print result[i]
	}
}

function part2() {
	#coord = "26:29"
	coord = "11:13"
	calculateAngles(result, coord, points)
}

function compareDistanceAndAngle(coordA, v1, coordB, v2,    l, r)
{
	print ""
	split(coordA, coordsA, ":")
    split(coordB, coordsB, ":")
    split(SORTPOINT, coordsO, ":")

	aX = int(coordsA[1])
	aY = int(coordsA[2])

	bX = int(coordsB[1])
	bY = int(coordsB[2])

	oX = int(coordsO[1])
	oY = int(coordsO[2])

	angleOA = atan2(-(aX-oX), -(aY-oY))
	distanceOA = (aY-oY)*(aY-oY) + (aX-oX)*(aX-oX) 

	printf "coord A %s. Angle %s, distance %s\n", coordA, angleOA, distanceOA

	angleOB = atan2(-(bX-oX), -(bY-oY))
	distanceOB = (bY-oY)*(bY-oY) + (bX-oX)*(bX-oX) 

	printf "coord B %s. Angle %s, distance %s\n", coordB, angleOB, distanceOB

	if (angleOA < angleOB) {
		print "return -1"
		return -1
	}

	if (angleOA > angleOB) {
		print "return 1"
		return 1
	}

	if (distanceOA < distanceOB) {
		print "return -1"
		return -1
	}

	if (distanceOA > distanceOB) {
		return 1
	}

	return 0
}


END {
	#part1()
	part2()
}