BEGIN {
	FS=","
}

{
	for (i = 1; i <= NF; i++) {
		data[i-1] = $i
	}

	count += NF
}

END {

	data[1] = 12
	data[2] = 2

	for (i=0; i< count; i += 4) {
		opcode = data[i]

		if (opcode == 99) {
			break
		}

		index1 = data[i+1]
		index2 = data[i+2]
		store = data[i+3]

		if (opcode == 1) {
			data[store] = data[index1] + data[index2]
		}

		if (opcode == 2) {
			data[store] = data[index1] * data[index2]
		}
	}

	print data[0]

}