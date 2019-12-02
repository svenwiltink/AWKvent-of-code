BEGIN {
	FS=","
}

{
	for (i = 1; i <= NF; i++) {
		data[i-1] = $i
	}

	count += NF
}

function copyArray(source, target, 		i) {
	for (i in source) {
		target[i] = source[i]
	}
}

function tryNounVerb(noun, verb, data,		dataCopy, i) {
	copyArray(data, dataCopy)

	dataCopy[1] = noun
	dataCopy[2] = verb

	for (i=0; i< count; i += 4) {
		opcode = dataCopy[i]

		if (opcode == 99) {
			break
		}

		index1 = dataCopy[i+1]
		index2 = dataCopy[i+2]
		store = dataCopy[i+3]

		if (opcode == 1) {
			dataCopy[store] = dataCopy[index1] + dataCopy[index2]
		}

		if (opcode == 2) {
			dataCopy[store] = dataCopy[index1] * dataCopy[index2]
		}
	}

	return dataCopy[0];	
}

END {

	requiredOutput = 19690720

	for (noun = 0; noun < 100; noun++) {
		for (verb = 0; verb < 100; verb++) {
			result = tryNounVerb(noun, verb, data)
			if (requiredOutput == result) {
				print 100 * noun + verb
				exit
			}
		}
	}
}