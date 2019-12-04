#!/usr/local/bin/awk -f

BEGIN {
	start = 271973
	end = 785961

	for (i = start; i <= end; i++) {
		split(i, digits, "")
		if (!checkDecrease(digits)) {
			continue
		}	

		if (!checkDouble(digits)) {
			continue
		}

		part1++

		if (checkFakeDoubles(digits)) {
			continue
		}

		part2++
	}

	printf "part1: %d\n", part1
	printf "part2: %d\n",part2
}

function checkDecrease(digits, 		i) {
	for (i in digits) {
		if (i == 1) {
			continue
		}

		if (digits[i] < digits[(i-1)]) {
			return 0
		}
	}

	return 1
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

function checkFakeDoubles(digits, 		i, encountered) {
	for (i in digits) {
		digit = digits[i]
		encountered[digit]++
	}

	for (i in encountered) {
		if (encountered[i] == 2) {
			return 0
		}
	}

	return 1
}