@include "intcode.awk"

END {
	copyArray(pdata, part1Data)
	copyArray(pdata, part2Data)

	part1(part1Data)
	#part2(part2Data)

	
	#printMap(screen, xMin, xMax, yMin, yMax)
}

function part1(pData) {
	drawTiles(screen, pData)

	for(i in screen) {
		if (screen[i] == 2) {
			blockCount++
		}
	}

	printf "Block count: %d\n", blockCount

	printMap(screen)
}

function part2(pData) {
	pData[0] = 2
	#drawTiles(screen, pData)
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

		x = int(x)
		y = int(y)

		mode = startIntCode(intCodeInstruction, intOptions, intVariables, intState)

		screen[x ":" y] = mode

		xMin = x < xMin ? x : xMin
		xMax = x > xMax ? x : xMax

		yMin = y < yMin ? y : yMin
		yMax = y > yMax ? y : yMax
	}
}

function printMap(map, 	x, y, currentColour) {
	printf "\033[2J"
	for (y=yMin; y<=yMax; y++) {
		for (x=xMin; x<=xMax;x++) {
			currentColour = " "

			if ((x ":" y) in map) {
				mode = map[x ":" y]
				if (mode == 1) {
					currentColour = "1"
				}

				if (mode == 2) {
					currentColour = "#"
				}

				if (mode == 3) {
					currentColour = "-"
				}


				if (mode == 4) {
					currentColour = "O"
				}
			}			

			printf currentColour
		}

		print ""
	}
}