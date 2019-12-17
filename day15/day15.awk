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
	first = 1
	for (key in state) {
		if (first) {
			buffer = buffer key ":" state[key]
			first = 0
		} else {
			buffer = buffer ":" key ":" state[key]
		}
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

	xMin = 0
	xMax = 0
	yMin = 0
	yMax = 0

	map["0:0"] = 1

	# abuse the state of the intcode machine to store data we want to save 
	# alongside the intcode instructions

	testState["xCoord"] = 0
	testState["yCoord"] = 0
	testState["steps"] = 0

	stateCount = 0
	states[stateCount] = stateToString(intCodeInstruction, testState)

	x = 0
	y = 0

	stepsToOxygenStation = 0
	foundOxygen = 0
	minutes = 0
	while (1) {
		statusRun = 0
		for (stateNr in states) {
			statusRun++

			stringToState(states[stateNr], intCodeInstruction, intState)
			x = intState["xCoord"]
			y = intState["yCoord"]

			# TODO. At each step, check what directions are available by trying to traverse them. If traversing the path was successful, move back. If multiple directions are available we have to start Breath First Search by adding the directions to the list of statemachines to execute

			alreadySavedState = 0
			deadEnd = 1
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
					continue
				}

				copyArray(intCodeInstruction, testInstructions)
				copyArray(intState, testState)

				intVariables[0] = newDirection

				result = startIntCode(testInstructions, intOptions, intVariables, testState)

				printf "%d %d would be %d. Using direction %d\n", newX, newY, result, newDirection

				map[newX ":" newY] = result

				xMin = newX < xMin ? newX : xMin
				xMax = newX > xMax ? newX : xMax

				yMin = newY < yMin ? newY : yMin
				yMax = newY > yMax ? newY : yMax

				testState["xCoord"] = newX
				testState["yCoord"] = newY
				testState["steps"]++
				
				if (result == 1) {
					if (found)
					printf "%d %d is AIR, can traverse\n", newX, newY
					if (!alreadySavedState) {
						printf "Updating current state\n"
						states[stateNr] = stateToString(testInstructions, testState)
						alreadySavedState = 1
					} else {
						printf "Adding new state"
						stateCount++
						states[stateCount] = stateToString(testInstructions, testState)
					}
				}

				# only care about the oxygen station once
				if (foundOxygen == 0 && result == 2) {
					stepsToOxygenStation = testState["steps"]
					foundOxygen = 1
					
					print "Found oxygen station. Resetting state"

					# we have found the oxygen station. This is now the _only_ valid state.
					# delete the other states so we can reset our search

					for (i in states) {
						delete states[i]
					}

					for (i in map) {
						delete map[i]
					}

					states[0] = stateToString(testInstructions, testState)
					stateCount = 1
					breakStateLoop = 1
					break
				}

				deadEnd = 0
			}


			if (breakStateLoop) {
				breakStateLoop = 0
				break
			}

			if (deadEnd) {
				print "Dead end. removing state"
				delete states[stateNr]
			}
		}

		if (statusRun == 0) {
			print "no more states to execute, exiting"
			break
		}

		if (foundOxygen) {
			minutes++
		}

		printMap(map, x, y)

	}

	# substract 2 because we count the step where we find the oxygen station
	# and when we find the last dead end
	minutes -= 2

	printf "%d steps to oxygen station\n", stepsToOxygenStation
	printf "%d minutes to fill up the haul\n", minutes
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

		print "\033[2J"
		#print "\n\n\n"
		print buffer
	
}