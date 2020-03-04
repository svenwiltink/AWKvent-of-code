@include "intcode.awk"

END {
	part1()
}

function part1() {
		copyArray(pdata, part1Data)
		drawMap(part1Data, map)
		printMap(map)
		alignment = calculateAlignment(map, xMax, yMax)
		printMap(map)
		print alignment
}

function calculateAlignment(map, xMax, yMax) {
	alignment = 0
	for (x=0; x <= xMax; x++) {
		for (y=0; y <= yMax; y++) {
			if (debug) printf "checking x %d y %d for an intersection\n", x, y

			if (map[x ":" y ] == 35 &&
				map[x ":" y+1] == 35 && 
				map[x+1 ":" y] == 35 && 
				map[x ":" y-1] == 35 &&
				map[x-1 ":" y] == 35) {
				if (debug) print "found one"

				map[x ":" y] = "O"

				intersectionAlignment = x * y
				if (debug) printf "algnment is %d\n", intersectionAlignment
				alignment += intersectionAlignment
			}
		}
	}

	return alignment
}

function drawMap(intCodeInstructions, map) {
	intOptions["return-on-print"] = 1

	x = 0
	y = 0

	xMin = 0
	xMax = 0
	yMin = 0
	yMax = 0

	while(1) {
		result = startIntCode(intCodeInstructions, intOptions, intVariables, intCodeState)
		
		if (intCodeState["HALT"]) {
			if (debug) print "halting intCode machine"
			return
		}

		xMin = x < xMin ? x : xMin
		xMax = x > xMax ? x : xMax

		yMin = y < yMin ? y : yMin
		yMax = y > yMax ? y : yMax

		if (result == 10) {
			x = 0
			y++
			continue
		}

		if (debug) printf "x: %d, y %d - %d\n", x, y, result
		map[x ":" y] = result

		x++
	}

}

# print the map. rX and rY are the robot coords
function printMap(map, rX, rY,		x, y, currentTile) {
		buffer = ""
		for (y=yMin; y<=yMax; y++) {
			for (x=xMin; x<=xMax;x++) {
				currentTile = " "

				if ((x ":" y) in map) {
					mode = map[x ":" y]
					print mode
					if (mode == 35) {
						currentTile = "#"
					}

					if (mode == 46) {
						currentTile = "."
					}

					if (mode =="O") {
						currentTile = "O"
					}

					if (x == rX && y == rY) {
						currentTile= "V"
					}

				}			

				buffer = buffer "" currentTile
			}

			buffer = buffer "\n"
		}

		#print "\033[2J"
		#print "\n\n\n"
		print buffer
	
}