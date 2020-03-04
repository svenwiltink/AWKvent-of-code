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

function generatePattern(position, digitCount, 		first, patternString, i, j, numbers) {
	if (position in patternStringCache) {
		return
	}

	split(basePattern, numbers, ", ")


	patternString = ""
	patternSign = ""
	nr = 1
	for (i in numbers) {
		for (j=1; j<= int(position); j++) {
				if (debug) printf "j: %d position: %d\n", j, position
				if (debug) printf "have to repeat %d times\n", position
				if (debug) printf "repeating number %d from base pattern. Value %d\n", i, int(numbers[i])

				d = numbers[i]				
				pattern[nr] = d
				nr++

				digitLength = length(d)

				if (digitLength == 2) {
					patternString = patternString substr(d, digitLength)
					patternSign = patternSign "-"
				} else {
					patternString = patternString d
					patternSign = patternSign "+"
				}
		}
	}

	patternStringCache[position] = patternString
	patternSignCache[position] = patternSign
	
}
function calculateDigit(position, digits, 		i, digit, pattern, newNumber, newNumberDigits) {
	if (debug) printf "generating number for position %d\n", position
	generatePattern(position, length(digits))

	newNumber = 0
	for (i in digits) {
		digit = digits[i]

		cachedPattern = patternStringCache[position]
		cachedSign = patternSignCache[position]
		offset = i%length(cachedPattern)+1

		sign = substr(cachedSign, offset, 1)
		number = substr(cachedPattern, offset, 1)

		newNr = int(sign number)
		if (debug) printf "Digit number: %d\n", i
		newNumber += int(digit) * newNr
		if (debug) printf "%d * %d = %d \n", digit, int(pattern[i+1]), newNumber
	}


	if (debug) printf "new digit: %d -> %d\n", newNumber, substr(newNumber, length(newNumber))

	return substr(newNumber, length(newNumber))
}