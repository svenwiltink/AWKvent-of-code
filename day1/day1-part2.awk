BEGIN {
	total = 0
}

{
   	mass = int(($1 / 3) - 2)
   	extraFuel = mass
	while (1 == 1) {
		extraFuel = int((extraFuel / 3) - 2)
		if (extraFuel <= 0) {
		   break
		}

		mass += extraFuel
	}

	total += mass
}

END {
	print total
}