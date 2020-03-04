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
					continue
				}

				ymin = vLine[2] < vLine[4] ? vLine[2] : vLine[4]
				ymax = vLine[2] > vLine[4] ? vLine[2] : vLine[4]

				hLineY = hLine[2]

				if (hLineY < ymin || hLineY > ymax) {
					continue
				}

				# the lines must cross now. The result will be the X of vertical line and the Y
				# of the horizontal line
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

		if (shortest == 0 || distance < shortest) {
			shortest = distance
			coord = intersections[i]
		}
	}

	return shortest
}

function getSteps(series, location, 		i, coords, target, distance) {
	split(location, loc, ":")

	for (i in series) {
		split(series[i], coords, ":")

		distance = abs(coords[4] - coords[2]) + abs(coords[3] - coords[1])

		# check if the next line contains the intersection
		# If our line is horizontal we need to share the same Y coordinate
		# If our line is vertical we need to share the same X coordinate
		# the bounds will also have to be checked
		horizontal = coords[1] != coords[3]
		if (horizontal == 1 && loc[2] != coords[4]) {
			steps += distance
			continue
		}

		if (horizontal == 0 && loc[1] != coords[3]) {
			steps += distance
			continue
		}

		# we are in the same plane. Do a bounds check. When we are horizontal we have to make sure
		# the X is in bounds. When vertical the Y must be in bounds

		xmin = coords[1] < coords[3] ? coords[1] : coords[3]
		xmax = coords[1] > coords[3] ? coords[1] : coords[3]
		ymin = coords[2] < coords[4] ? coords[2] : coords[4]
		ymax = coords[2] > coords[4] ? coords[2] : coords[4]

		if (horizontal && (loc[1] < xmin || loc[1] > xmax)) {
			steps += distance
			continue
		}

		if (!horizontal && (loc[2] < ymin || loc[2] > ymax)) {
			steps += distance
			continue
		}

		# the intersection is on this line. Calulate the distance and return
		steps += abs(loc[1] - coords[1]) + abs(loc[2] - coords[2])
		return
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
		calculateCoordinates(seriesA)
	}

	if (NR == 2) {
		calculateCoordinates(seriesB)
	}
}


END {
	findIntersections(seriesA, seriesB, intersections)
	shortest = findShortestDistance(intersections)

	print "Part 1: " shortest


	shortest = 0
	for (i in intersections) {
		intersection = intersections[i]
		if (intersection == "0:0") {
			continue
		}

		steps = 0
		getSteps(seriesA, intersection)
		getSteps(seriesB, intersection)

		if (shortest == 0 || steps < shortest) {
			shortest = steps
		}
	}

	print "Part 2: " shortest
}