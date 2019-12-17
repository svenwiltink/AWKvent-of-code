@include "intcode.awk"

END {
	part1()
}

function part1() {
		copyArray(pdata, part1Data)
		findPath(part1Data, map)
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
	while (1) {

		intVariables[0] = direction

		result = startIntCode(intCodeInstruction, intOptions, intVariables, intState)

		blockX = x
		blockY = y

		if (direction == 1) {
			blockY++
		}

		if (direction == 2) {
			blockY--
		}

		if (direction == 3) {
			blockX--
		}

		if (direction == 4) {
			blockX++
		}

		# we've moved forward. Update x and y
		if (result == 1) {
			x = blockX
			y = blockY
		}

		xMin = blockX < xMin ? blockX : xMin
		xMax = blockX > xMax ? blockX : xMax

		yMin = blockY < yMin ? blockY : yMin
		yMax = blockY > yMax ? blockY : yMax

		# TODO. At each step, check what directions are available by trying to traverse them. If traversing the path was successful, move back. If multiple directions are available we have to start Breath First Search by adding the directions to the list of statemachines to execute


		printf "Setting x %d y %d to %d\n", blockX, blockY, result
		map[blockX ":" blockY] = result

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