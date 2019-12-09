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
	pvariables[0] = 1
	startIntCode(pdata, pvariables, pstate)
	for (p in pstate) {
		print p, pstate[p]
	}
}