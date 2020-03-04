BEGIN {
	FS=","
	debug = 1
}

{
	for (i = 1; i <= NF; i++) {
		data[i-1] = $i
	}

	count += NF
}

END {
	pc = 0

	#data[1] = 12
	#data[2] = 2

	while (pc < count) {
		instruction = data[pc]
		instruction = sprintf("%05d", instruction)
		match(instruction, "^([0-9]{1})([0-9]{1})([0-9]{1})([0-9]{1})([0-9]{1})$", matches)

		opcode = int(matches[4] "" matches[5])

		p1Mode = matches[3]
		p2Mode = matches[2]
		p3Mode = matches[1]


		if (opcode == 99) {
			break
		}

		p1Index = data[pc+1]
		p2Index = data[pc+2]
		p3Index = data[pc+3]

		p1 = p1Mode == 1 ? p1Index : data[p1Index]
		p2 = p2Mode == 1 ? p2Index : data[p2Index]
		p3 = p3Mode == 1 ? p3Index : data[p3Index]

		if (debug) {
			print ""
			printf "PC: %d\n", pc
			printf "instruction: %s\n", instruction
			printf "data %d %d %d %d\n", data[pc], data[pc+1], data[pc+2], data[pc+3]
			printf "opcode: %d. p1 %s, p2: %s p3: %d\n", opcode, p1, p2, p3
		}
		
		if (opcode == 99) {
			print "WE ARE DONE!"
			break
		}

		# add
		if (opcode == 1) {
			location = data[pc+3]
			newValue = p1 + p2

			printf "storing %d in %d\n", newValue, location
			data[location] = newValue
			pc += 4
			continue
		}

		# multiply
		if (opcode == 2) {
			location = data[pc+3]
			newValue = p1 * p2

			printf "storing %d in %d\n", newValue, location
			data[location] = newValue
			pc += 4
			continue
		}

		if (opcode == 3) {
			printf "Enter a number: "
			banana = readStdin()
			if (debug) {
				printf "Storing %d in location %d\n", banana, data[pc+1]
			}
			data[data[pc+1]] = banana
			pc += 2
			continue
		}

		if (opcode == 4) {
			print ""
			printf "DIAGNOSTICS %d\n", data[p1Index]
			print ""
			pc += 2
			continue
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
			location = data[pc+3]
			value = p1 < p2

			printf "storing %d in %d", value, location
			data[location] = value

			pc += 4
			continue
		}	

		# equal
		if (opcode == 8) {
			location = data[pc+3]
			value = p1 == p2

			printf "storing %d in %d", value, location
			data[location] = value

			pc += 4
			continue
		}			
	}

	print "end"

}

function readStdin()
{
	file = "/dev/stdin"
    getline contents < file
    return contents
}