@include "intcode.awk"
BEGIN {
	FS=","
}

{
	for (i = 1; i <= NF; i++) {
		pdata[i-1] = $i
	}

	count += NF
}

END {
	copyArray(pdata, part1Data)
	copyArray(pdata, part2Data)

	part1()
	#part2()
}

function part1() {
	part1Map["0:0"] = 0
	paint(part1Map, part1Data)

	for (i in part1Map) {
		painted++
	}

	printf "total panels: %d\n", painted
}

function part2() {
	part2Map["0:0"] = 0
	#displayMap = 1
	paint(part2Map, part2Data)

	painted = 0 
	for (i in part2Map) {
		painted++
	}

	printf "total panels: %d\n", painted
}

function paint(map, intCodeInstruction, 	x, y, part1State) {

	intOptions["return-on-print"] = 1
	

	x = 0
	y = 0

	currentDirection = "up"


	xMin = 0
	xMax = 0
	yMin = 0
	yMax = 0

	# keep running until it halts
	while (part1State["HALT"] == 0) {

		xMin = x < xMin ? x : xMin
		xMax = x > xMax ? x : xMax

		yMin = y < xMin ? y : xMin
		yMax = y > xMax ? y : xMax

		currentColour = int(map[x ":" y])
		
		if (debug) printf "x: %d, y: %d\n", x, y
		if (debug) printf "colour: %d\n", currentColour
		if (debug) printf "direction: %s\n", currentDirection

		intVariables[0] = currentColour

		newColour = startIntCode(intCodeInstruction, intOptions, intVariables, part1State)
		rotation =  startIntCode(intCodeInstruction, intOptions, intVariables, part1State)

		if (part1State["HALT"]) {
			break
		}
		
		map[x ":" y] = newColour

		if (debug) printf "colour: %d, rotation: %d\n", newColour, rotation

		if (rotation == 0) {
			if (currentDirection == "up") {
				currentDirection = "left"
				x--
				continue
			}
			if (currentDirection == "left") {
				currentDirection = "down"
				y--
				continue
			}
			if (currentDirection == "down") {
				currentDirection = "right"
				x++
				continue
			}
			if (currentDirection == "right") {
				currentDirection = "up"
				y++
				continue
			}
		}

		if (currentDirection == "up") {
			currentDirection = "right"
			x++
			continue
		}
		if (currentDirection == "right") {
			currentDirection = "down"
			y--
			continue
		}
		if (currentDirection == "down") {
			currentDirection = "left"
			x--
			continue
		}		
		if (currentDirection == "left") {
			currentDirection = "up"
			y++
			continue
		}
	}

	if (displayMap) {
			printMap(map, xMin, xMax, yMin, yMax)
	}
}

function printMap(map, xMin, xMax, yMin, yMax,		x, y, currentColour) {
	for (y=yMax; y>=0; y--) {
		for (x=xMin; x<=xMax;x++) {
			currentColour = "."

			if ((x ":" y) in map) {
				currentColour = int(map[x ":" y]) == 0 ? "0" : "#"
			}			

			printf currentColour
		}

		print ""
	}
}