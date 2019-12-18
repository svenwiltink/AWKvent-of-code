BEGIN {
	basePattern = "0, 1, 0, -1"
}

{
	number = $1
	for (i=0; i<100; i++) {
		number = applyPhase(number)
		print number "\n\n"
	}

}

function applyPhase(number, 		i, digits, newDigits, returnValue) {
	split(number, digits, "")
	for (i in digits) {
		returnValue = returnValue calculateDigit(i, digits)
	}

	return returnValue
}

function generatePattern(position, pattern, digitCount, 		first, patternString, i, j, numbers) {
	split(basePattern, numbers, ", ")

	patternString = ""
	first = 1
	for (i in numbers) {
		for (j=1; j<= int(position); j++) {
				if (debug) printf "j: %d position: %d\n", j, position
				if (debug) printf "have to repeat %d times\n", position
				if (debug) printf "repeating number %d from base pattern. Value %d\n", i, int(numbers[i])
				if (first) {
					patternString = numbers[i]
					first = 0
				} else {
					patternString = patternString " " numbers[i]
				}
				if (debug) printf "new pattern: %s\n", patternString
		}
	}

	while ((length(patternString) - 100) <= (digitCount * 3)) {
		patternString = patternString " " patternString
	}

	split(patternString, pattern, " ")
}
function calculateDigit(position, digits, 		i, digit, pattern, newNumber, newNumberDigits) {
	if (debug) printf "generating number for position %d\n", position
	elementCount = generatePattern(position, pattern, length(digits))
	if (debug) printf "generated pattern: "
	for (i in pattern) {
		if (debug) printf "%s, ", pattern[i]
	} 

	newNumber = 0
	for (i in digits) {
		digit = digits[i]

		if (debug) printf "Digit number: %d\n", i
		newNumber += int(digit) * int(pattern[i+1])
		if (debug) printf "%d * %d = %d \n", digit, int(pattern[i+1]), newNumber
	}


	elementCount = split(newNumber, newNumberDigits, "")
	if (debug) printf "new digit: %d -> %d\n", newNumber, newNumberDigits[elementCount]

	return newNumberDigits[elementCount]
}