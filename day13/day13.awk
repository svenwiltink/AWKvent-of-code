@include "intcode.awk"

END {
	copyArray(pdata, part1Data)

	part1(part1Data)
}

function part1(pData) {
	drawTiles(screen, pData)

	for(i in screen) {
		if (screen[i] == 2) {
			blockCount++
		}
	}

	printf "Block count: %d\n", blockCount
}

function drawTiles(screen, intCodeInstruction,		intOptions, intVariables, intState) {

	intOptions["return-on-print"] = 1

	xMin = 0
	xMax = 0
	yMin = 0
	yMax = 0

	while (intState["HALT"] != 1) {
		x = startIntCode(intCodeInstruction, intOptions, intVariables, intState)
		y = startIntCode(intCodeInstruction, intOptions, intVariables, intState)

		mode = startIntCode(intCodeInstruction, intOptions, intVariables, intState)

		screen[x ":" y] = mode

		xMin = x < xMin ? x : xMin
		xMax = x > xMax ? x : xMax

		yMin = y < xMin ? y : xMin
		yMax = y > xMax ? y : xMax
	}

	printMap(screen, xMin, xMax, yMin, yMax)
}

function printMap(map, xMin, xMax, yMin, yMax,		x, y, currentColour) {
	for (y=yMax; y>=yMin-1; y--) {
		for (x=xMin; x<=xMax;x++) {
			currentColour = "."

			if ((x ":" y) in map) {
				currentColour = map[x ":" y]
			}			

			printf currentColour
		}

		print ""
	}
}