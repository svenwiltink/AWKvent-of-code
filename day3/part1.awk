# Every line is scanned for directions, which are turned into coordinates. Together with the 
# Previous coordinate we form a line, which are stored in series. In the series the lines are
# stored in the following format: x1:y1:x2:y2
# The coordinates start at 0:0. The absolute value doesn't matter as we only care about the relative distance
BEGIN {
	FS=","
}

function calculateCoordinates(target) {
	x = 0
	y = 0


	prevX = 0
	prevY = 0
	for (i=1; i <= NF; i++) {
		direction = substr($i, 1, 1)
		amount = substr($i, 2)

		printf "direction %s, amount %d\n", direction, amount

		if (direction == "U") {
			y += amount
		}

		if (direction == "D") {
			y -= amount
		}

		if (direction == "L") {
			x -= amount
		}

		if (direction == "R") {
			x += amount
		}

		printf "current location X: %d Y: %d\n", x, y

		target[i] = prevX ":" prevY ":" x ":" y 

		prevX = x
		prevY = y
	}
}


# because we are dealing only with vertical and horizontal lines we don't need to do create
# formulas and solve them. We can simply check the direction of the lines and do a bounds
# check.
function findIntersections(seriesA, seriesB, result) {
	currentResultIndex = 0

	for (i in seriesA) {
		for (j in seriesB) {
			split(seriesA[i], aCoords, ":")
			split(seriesB[j], bCoords, ":")

			# if X changes the same the line is horizontal. If both series have the same
			# orientation they must be parallel and will never intersect
			aDirection = aCoords[1] != aCoords[3]
			bDirection = bCoords[1] != bCoords[3]

			if (aDirection != bDirection) {
				#now we do a bounds check. First we determine the min and max coords

				if (aDirection == 1) {
					copyArray(aCoords, hLine)
					copyArray(bCoords, vLine)
				}

				if (bDirection == 1) {
					copyArray(bCoords, hLine)
					copyArray(aCoords, vLine)
				}

				xmin = hLine[1] < hLine[3] ? hLine[1] : hLine[3]
				xmax = hLine[1] > hLine[3] ? hLine[1] : hLine[3]

				vLineX = vLine[1] 

				# make sure the vertical line is between the bounds of the horizontal line
				if (vLineX < xmin || vLineX > xmax) {
					print "lines don't cross. X out of bounds"
					continue
				}

				ymin = vLine[2] < vLine[4] ? vLine[2] : vLine[4]
				ymax = vLine[2] > vLine[4] ? vLine[2] : vLine[4]

				hLineY = hLine[2]

				if (hLineY < ymin || hLineY > ymax) {
					print "lines don't cross. Y out of bounds"
					continue
				}

				# the lines must cross now. The result will be the X of vertical line and the Y
				# of the horizontal line

				printf "intersection found: x %d y %d\n", vLineX, hLineY

				result[currentResultIndex] = vLineX ":" hLineY
				currentResultIndex++
			}
		}
	}
}

function findShortestDistance(intersections) {
	shortest = 0
	for (i in intersections) {
		split(intersections[i], coords, ":")
		distance = abs(coords[1]) + abs(coords[2])

		printf "distance to %d %d is %d\n", coords[1], coords[2], distance

		if (shortest == 0 || distance < shortest) {
			shortest = distance
			coord = intersections[i]
		}
	}

	return shortest
}

function getSteps(series, location, 	coords) {
	for (i in series) {
		split(series[i], coords, ":")

		# check if the next line contains the intersection
	}
}

function abs(v) {
	return v < 0 ? -v : v
}

function copyArray(source, target, 		i) {
	for (i in source) {
		target[i] = source[i]
	}
}

{
	if (NR == 1) {
		print "calculating coordinates for A"
		calculateCoordinates(seriesA)
	}

	if (NR == 2) {
	print "calculating coordinates for B"
		calculateCoordinates(seriesB)
	}
}


END {
	findIntersections(seriesA, seriesB, intersections)
	shortest = findShortestDistance(intersections)

	print shortest

	getSteps(seriesA, coord)
	getSteps(seriesB, coord)

	print steps
}