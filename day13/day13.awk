@include "intcode.awk"
@load "time"

END {
	copyArray(pdata, part1Data)
	copyArray(pdata, part2Data)

	part1(part1Data)
	part2(part2Data)

}

function part1(pData) {
	playGame(screen, pData)

	for(i in screen) {
		if (screen[i] == 2) {
			blockCount++
		}
	}

	printf "Block count: %d\n", blockCount
}

function part2(pData) {
	pData[0] = 2
	playGame(screen, pData)
	printf "score: %d\n", screen["-1:0"]
}

function playGame(screen, intCodeInstruction,		intOptions, intVariables, intState) {

	intOptions["return-on-print"] = 1

	xMin = 0
	xMax = 0
	yMin = 0
	yMax = 0

	ballX = 0
	paddleX = 0

	while (intState["HALT"] != 1) {
		intVariables[0] = 0

		if (paddleX < ballX) {
			intVariables[0] = 1
		}

		if (paddleX > ballX) {
			intVariables[0] = -1
		}

		x = startIntCode(intCodeInstruction, intOptions, intVariables, intState)
		y = startIntCode(intCodeInstruction, intOptions, intVariables, intState)

		x = int(x)
		y = int(y)


		mode = startIntCode(intCodeInstruction, intOptions, intVariables, intState)

		if (mode == 4) {
			ballX = x
		}

		if (mode == 3) {
			paddleX = x
		}

		screen[x ":" y] = mode

		xMin = x < xMin ? x : xMin
		xMax = x > xMax ? x : xMax

		yMin = y < yMin ? y : yMin
		yMax = y > yMax ? y : yMax

		if (shouldPrint) printMap(screen)
	}
}

function printMap(map, 	x, y, currentColour) {
	frameCount = frameCount % 10
	frameCount++

	if (frameCount == 1) {
		buffer = "Score: " map["-1:0"] " \n\n"
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
						currentColour = "_"
					}


					if (mode == 4) {
						currentColour = "O"
					}
				}			

				buffer = buffer "" currentColour
			}

			buffer = buffer "\n"
		}

		print "\033[2J"
		print buffer
	}

	sleep("0.001")
}