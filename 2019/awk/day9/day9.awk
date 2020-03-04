@include "intcode.awk"
BEGIN {
	FS=","
}

{
	for (i = 1; i <= NF; i++) {
		pdata[i-1] = $i
	}

	count += NF
}

END {

	copyArray(pdata, part1Data)
	copyArray(pdata, part2Data)

	part1Variables[0] = 1
	print startIntCode(part1Data, part1Variables, part1State)

	part2Variables[0] = 2
	print startIntCode(part2Data, part2Variables, part2State)
}