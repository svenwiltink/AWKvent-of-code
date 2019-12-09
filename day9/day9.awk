@include "intcode.awk"
BEGIN {
	FS=","
	debug = 1
}

{
	for (i = 1; i <= NF; i++) {
		pdata[i-1] = $i
	}

	count += NF
}

END {
	startIntCode(pdata, ampVariables, ampAState)
}