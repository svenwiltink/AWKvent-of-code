BEGIN {
	FS=","
	debug = 0
}

{
	for (i = 1; i <= NF; i++) {
		pdata[i-1] = $i
	}

	count += NF
}

END {
	part1()
}

function part1(			generatedSettings) {
	generateP1Settings(generatedSettings)	
	startWithSettings(generatedSettings)

	printf "Max output %s with %settings", max, maxSettings
}

function startWithSettings(generatedSettings) {
	max = 0
	for (settingsString in generatedSettings) {
		printf "trying settings %s\n", settingsString
		split(settingsString, settings, "")


		# reset the amp data and program counters
		copyArray(pdata, ampAData)
		copyArray(pdata, ampBData)
		copyArray(pdata, ampCData)
		copyArray(pdata, ampDData)
		copyArray(pdata, ampEData)


		ampAState["pc"] = 0
		ampBState["pc"] = 0
		ampCState["pc"] = 0
		ampDState["pc"] = 0
		ampEState["pc"] = 0

		ampOutput = 0
		for (i=1; i<= 5; i++) {
			if (debug) {
				printf "starting AMP %s with settings %s and input %s", i, settings[i], ampInput
			}

			ampVariables[0] = settings[i]
			ampVariables[1] = ampOutput

			copyArray(pdata, data)

			if (i == 1) ampOutput = startProgram(ampAData, ampVariables, ampAState)
			if (i == 2) ampOutput = startProgram(ampBData, ampVariables, ampBState)
			if (i == 3) ampOutput = startProgram(ampCData, ampVariables, ampCState)
			if (i == 4) ampOutput = startProgram(ampDData, ampVariables, ampDState)
			if (i == 5) ampOutput = startProgram(ampEData, ampVariables, ampEState)
		}

		printf "output: %s\n", ampOutput

		if (int(ampOutput) > int(max)) {
			max = ampOutput
			maxSettings = settingsString
		}
	}
}

# super lazy way to generate the settings
function generateP1Settings(result,		i) {
	start = 01234
	end = 43210

	for (i = start; i <= end; i++) {
		setting = sprintf("%05d", i)
		split(setting, digits, "")

		if (match(i, "[5-9]", matches)) {
			continue
		}

		if (checkDouble(digits)) {
			continue
		}

		result[setting] = 1
	}
}

function checkDouble(digits, 		i, encountered) {
	for (i in digits) {
		digit = digits[i]

		if (digit in encountered) {
			return 1
		}

		encountered[digit] = 1
	}

	return 0
}


function copyArray(source, target, 		i) {
	for (i in source) {
		target[i] = source[i]
	}
}

function startProgram(ampData, variables, ampState) {
	pc = int(ampState["pc"])
	variableIndex = 0
	outputString = ""

	while (pc < count) {
		instruction = ampData[pc]
		instruction = sprintf("%05d", instruction)
		match(instruction, "^([0-9]{1})([0-9]{1})([0-9]{1})([0-9]{1})([0-9]{1})$", matches)

		opcode = int(matches[4] "" matches[5])

		p1Mode = matches[3]
		p2Mode = matches[2]
		p3Mode = matches[1]


		if (opcode == 99) {
			return outputString
			break
		}

		p1Index = ampData[pc+1]
		p2Index = ampData[pc+2]
		p3Index = ampData[pc+3]

		p1 = p1Mode == 1 ? p1Index : ampData[p1Index]
		p2 = p2Mode == 1 ? p2Index : ampData[p2Index]
		p3 = p3Mode == 1 ? p3Index : ampData[p3Index]

		if (debug) {
			print ""
			printf "PC: %d\n", pc
			printf "instruction: %s\n", instruction
			printf "data %d %d %d %d\n", ampData[pc], ampData[pc+1], ampData[pc+2], ampData[pc+3]
			printf "opcode: %d. p1 %s, p2: %s p3: %d\n", opcode, p1, p2, p3
		}

		# add
		if (opcode == 1) {
			location = ampData[pc+3]
			newValue = p1 + p2

			if (debug) printf "storing %d in %d\n", newValue, location
			ampData[location] = newValue
			pc += 4
			continue
		}

		# multiply
		if (opcode == 2) {
			location = ampData[pc+3]
			newValue = p1 * p2

			if (debug) printf "storing %d in %d\n", newValue, location
			ampData[location] = newValue
			pc += 4
			continue
		}

		if (opcode == 3) {
			banana = variables[variableIndex]
			
			if (debug) {
				printf "reading variableIndex %d\n", variableIndex
				printf "Storing %d in location %d\n", banana, ampData[pc+1]
			}

			variableIndex++
			ampData[ampData[pc+1]] = banana
			pc += 2
			continue
		}

		if (opcode == 4) {
			if (debug) print ""
			if (debug) printf "DIAGNOSTICS %d\n", ampData[p1Index]
			if (debug) print ""

			outputString = outputString "" ampData[p1Index]

			pc += 2

			ampState["pc"] = pc
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
			location = ampData[pc+3]
			value = p1 < p2

			printf "storing %d in %d", value, location
			ampData[location] = value

			pc += 4
			continue
		}	

		# equal
		if (opcode == 8) {
			location = ampData[pc+3]
			value = p1 == p2

			printf "storing %d in %d", value, location
			ampData[location] = value

			pc += 4
			continue
		}			
	}
}