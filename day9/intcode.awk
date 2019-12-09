
function resetIntCode(pState) {
	pState["pc"] = 0
	pState["rc"] = 0
	pState["HALT"] = 0
}

function copyArray(source, target, 		i) {
	for (i in source) {
		target[i] = source[i]
	}
}

# startIntCode starts an intcode machine.
# pDate contains the program data,
# variables contains the variables to read when opcode 3 is encountered
# pState will contain the program counter (pc), relative counter (rc) and HALT keys when returned
# pState can be fed back into startIntCode to resume a previous state
function startIntCode(pData, variables, pState) {
	pc = int(pState["pc"])
	rc = int(pState["rc"])
	variableIndex = 0
	outputString = ""

	while (pc < count) {
		instruction = pData[pc]
		instruction = sprintf("%05d", instruction)
		match(instruction, "^([0-9]{1})([0-9]{1})([0-9]{1})([0-9]{1})([0-9]{1})$", matches)

		opcode = int(matches[4] "" matches[5])

		p1Mode = matches[3]
		p2Mode = matches[2]
		p3Mode = matches[1]

		p1Index = pData[pc+1]
		p2Index = pData[pc+2]
		p3Index = pData[pc+3]

		p1 = p1Mode == 1 ? p1Index : pData[p1Index]
		p2 = p2Mode == 1 ? p2Index : pData[p2Index]
		p3 = p3Mode == 1 ? p3Index : pData[p3Index]

		p1 = p1Mode == 2 ? pData[p1Index + rc] : p1
		p2 = p2Mode == 2 ? pData[p2Index + rc] : p2
		p3 = p3Mode == 2 ? pData[p3Index + rc] : p3

		if (debug) {
			print ""
			printf "PC: %d\n", pc
			printf "RC: %d\n", rc
			printf "instruction: %s\n", instruction
			printf ""
			printf "data %d %d %d %d\n", pData[pc], pData[pc+1], pData[pc+2], pData[pc+3]
			printf "opcode: %d. p1 %s, p2: %s p3: %d\n", opcode, p1, p2, p3
		}

		if (opcode == 99) {
			pState["HALT"] = 1
			return outputString
		}

		# add
		if (opcode == 1) {
			location = p3Index
			location = p3Mode == 2 ? location + rc : location

			newValue = p1 + p2

			if (debug) printf "storing %d in %d\n", newValue, location
			pData[location] = newValue
			pc += 4
			continue
		}

		# multiply
		if (opcode == 2) {
			location = p3Index
			location = p3Mode == 2 ? location + rc : location

			newValue = p1 * p2

			if (debug) printf "storing %d in %d\n", newValue, location
			pData[location] = newValue
			pc += 4
			continue
		}

		if (opcode == 3) {

			banana = variables[variableIndex]

			location = p1Index
			location = p1Mode == 2 ? location + rc : location
			
			if (debug) {
				printf "reading variableIndex %d\n", variableIndex
				printf "Storing %d in location %d\n", banana, location
			}

			variableIndex++
			pData[location] = banana
			pc += 2
			continue
		}

		if (opcode == 4) {
			if (debug) print ""
			if (debug) printf "DIAGNOSTICS %d\n", p1
			if (debug) print ""

			outputString = outputString "" p1

			pc += 2

			pState["pc"] = pc
			continue
		}

		# jump-if-true
		if (opcode == 5) {
			if (p1 > 0) {

				if (debug) {
					printf "jumping to %d because %d > 0\n", p2, p1
				}
				pc = p2
				continue
			}

			pc += 3
			continue
		}

		# jump-if-false
		if (opcode == 6) {
			if (p1 == 0) {
				if (debug) printf "jumping to %d because %d == 0\n", p2, p1
				pc = p2
				continue
			}

			pc += 3
			continue
		}	

		# less than
		if (opcode == 7) {
			location = p3Index
			location = p3Mode == 2 ? location + rc : location

			value = p1 < p2

			if (debug) printf "storing %d in %d\n", value, location
			pData[location] = value

			pc += 4
			continue
		}	

		# equal
		if (opcode == 8) {
			location = p3Index
			location = p3Mode == 2 ? location + rc : location
			value = p1 == p2

			if (debug) printf "storing %d in %d\n", value, location
			pData[location] = value

			pc += 4
			continue
		}

		if (opcode == 9) {
			rc += p1
			if (debug) printf "increasing rc with %d, now %d\n", p1, rc
			pc += 2
		}			
	}
}