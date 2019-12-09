
function resetIntCode(pState) {
	pState["pc"] = 0
	pState["rc"] = 0
	pState["HALT"] = 0
}

# startIntCode starts an intcode machine.
# pDate contains the program data,
# variables contains the variables to read when opcode 3 is encountered
# pState will contain the program counter (pc), relative counter (rc) and HALT keys when returned
# pState can be fed back into startIntCode to resume a previous state
#
# by default opcode 4 (print) will make the function return
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


		if (opcode == 99) {
			pState["HALT"] = 1
			return outputString
		}

		p1Index = pData[pc+1]
		p2Index = pData[pc+2]
		p3Index = pData[pc+3]

		p1 = p1Mode == 1 ? p1Index : pData[p1Index]
		p2 = p2Mode == 1 ? p2Index : pData[p2Index]
		p3 = p3Mode == 1 ? p3Index : pData[p3Index]

		if (debug) {
			print ""
			printf "PC: %d\n", pc
			printf "instruction: %s\n", instruction
			printf "data %d %d %d %d\n", pData[pc], pData[pc+1], pData[pc+2], pData[pc+3]
			printf "opcode: %d. p1 %s, p2: %s p3: %d\n", opcode, p1, p2, p3
		}

		# add
		if (opcode == 1) {
			location = pData[pc+3]
			newValue = p1 + p2

			if (debug) printf "storing %d in %d\n", newValue, location
			pData[location] = newValue
			pc += 4
			continue
		}

		# multiply
		if (opcode == 2) {
			location = pData[pc+3]
			newValue = p1 * p2

			if (debug) printf "storing %d in %d\n", newValue, location
			pData[location] = newValue
			pc += 4
			continue
		}

		if (opcode == 3) {

			banana = variables[variableIndex]
			
			if (debug) {
				printf "reading variableIndex %d\n", variableIndex
				printf "Storing %d in location %d\n", banana, pData[pc+1]
			}

			variableIndex++
			pData[pData[pc+1]] = banana
			pc += 2
			continue
		}

		if (opcode == 4) {
			if (debug) print ""
			if (debug) printf "DIAGNOSTICS %d\n", pData[p1Index]
			if (debug) print ""

			outputString = outputString "" pData[p1Index]

			pc += 2

			pState["pc"] = pc
			return outputString
		}

		# jump-if-true
		if (opcode == 5) {
			if (p1 > 0) {

				if (debug) {
					printf "jumping to %d because %d > 0", p2, p1
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
				if (debug) {
					printf "jumping to %d because %d == 0", p2, p1
				}
				pc = p2
				continue
			}

			pc += 3
			continue
		}	

		# less than
		if (opcode == 7) {
			location = pData[pc+3]
			value = p1 < p2

			printf "storing %d in %d", value, location
			pData[location] = value

			pc += 4
			continue
		}	

		# equal
		if (opcode == 8) {
			location = pData[pc+3]
			value = p1 == p2

			printf "storing %d in %d\n", value, location
			pData[location] = value

			pc += 4
			continue
		}

		if (opcode == 9) {
			rc += p1
			printf "increasing rc with %d, now %d\n", p1, rc
			pc += 2
		}			
	}
}