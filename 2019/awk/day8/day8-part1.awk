BEGIN {
	RS="[0-9]{150}"
	firstMatch = 1
}

{
	split(RT, digits, "")
	for (i in digits) {
		count[NR "-" digits[i]]++
	}

	zeroes = count[NR "-0"]

	if (zeroes > 0 && (firstMatch || zeroes < fewest)) {
		firstMatch = 0
		fewest = zeroes
		fewestLine = NR
	}
}

END {
	print count[fewestLine "-1"] * count[fewestLine "-2"]
}