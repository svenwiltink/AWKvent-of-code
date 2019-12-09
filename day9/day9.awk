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
	pvariables[0] = 1
	print startIntCode(pdata, pvariables, pstate)
}