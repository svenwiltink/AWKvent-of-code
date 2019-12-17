@include "intcode.awk"

END {
	part1()
}

function part1() {
		copyArray(pdata, part1Data)
		findPath(part1Data, map)
}

# status = stateString|instructions
# stateString = key:value:key:value
# instructions = comma separated ints
function stateToString(instructions, state, 		buffer, i, first) {
	for (key in state) {
		buffer = buffer key ":" state[key]
	}

	buffer = buffer "|"

	first = 1
	for (i in instructions) {
		if (first) {
			buffer = buffer instructions[i]
			first = 0
		} else {
			buffer = buffer "," instructions[i]
		}
	}

	return buffer
}

function stringToState(string, instructions, state, 	parts, stateString, instructionsString, key, newState, newInstructions) {
	split(string, parts, "|")
	stateString = parts[1]
	instructionString = parts[2]


	partCount = split(stateString, stateParts, ":")
	for (i=1; i <= partCount; i += 2) {
		newState[stateParts[i]] = stateParts[i+1]
	}

	for (key in state) {
		state[key] = newState[key]
	}

	for (key in newState) {
		state[key] = newState[key]
	}

	for (i in instructions) {
		delete instructions[i]
	}

	split(instructionString, newInstructions, ",")
	for (i in newInstructions) {
		instructions[i-1] = newInstructions[i]
	}

}

function findPath(intCodeInstruction, map) {

	intOptions["return-on-print"] = 1

	# 1 up
	# 2 down
	# 3 left
	# 4 right
	direction = 4

	x = 0
	y = 0

	xMin = 0
	xMax = 0
	yMin = 0
	yMax = 0

	map["0:0"] = 1


	stateCount = 0
	savedState =  stateToString(intCodeInstruction, testState)
	stringToState(savedState, newInstructions, newState)
	newSavedState =  stateToString(newInstructions, newState)

	print savedState "\n\n"
	print newSavedState
	states[0] = stateToString(intCodeInstruction, testState)


	while (1) {
		# TODO. At each step, check what directions are available by trying to traverse them. If traversing the path was successful, move back. If multiple directions are available we have to start Breath First Search by adding the directions to the list of statemachines to execute

		for (newDirection=1; newDirection <=4; newDirection++) {
			newX = x
			newY = y

			if (newDirection == 1) {
				newY++
			}

			if (newDirection == 2) {
				newY--
			}

			if (newDirection == 3) {
				newX--
			}

			if (newDirection == 4) {
				newX++
			}

			if (newX ":" newY in map) {
				printf "Already mapped %d %d, skipping\n", newX, newY
			}

			copyArray(intCodeInstruction, testInstructions)
			copyArray(intState, testState)

			intVariables[0] = newDirection

			result = startIntCode(testInstructions, intOptions, intVariables, testState)

			printf "%d %d would be %d. Using direction %d\n", newX, newY, result, newDirection

			map[newX ":" newY] = result

			xMin = blockX < xMin ? blockX : xMin
			xMax = blockX > xMax ? blockX : xMax

			yMin = blockY < yMin ? blockY : yMin
			yMax = blockY > yMax ? blockY : yMax
		}

		printMap(map, x, y)
	}
}


function printMap(map, rX, rY,		x, y, currentTile) {
		buffer = ""
		for (y=yMin; y<=yMax; y++) {
			for (x=xMin; x<=xMax;x++) {
				currentTile = " "

				if ((x ":" y) in map) {
					mode = map[x ":" y]
					if (mode == 0) {
						currentTile = "#"
					}

					if (mode == 1) {
						currentTile = "."
					}

					if (mode == 2) {
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
		print "\n\n\n"
		print buffer
	
}