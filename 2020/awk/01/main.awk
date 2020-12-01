{
	rem = 2020 - $1
	if (num[rem] != "") {
		print rem * $1
	}	

	num[$1] = 1
	numbers[NR] = $1
}

END {
	for (x in numbers) {
		for (y in numbers) {
			rem = 2020 - numbers[x] - numbers[y]
			if (num[rem] != "") {
				print numbers[x] * numbers[y] * rem
				exit
			}
		}
	}
}
