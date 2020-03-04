# each planet is stored as x:y:z:vx:vy:vz
{
	planets[NR] = int($1) ":" int($2) ":" int($3) ":0:0:0"
}

END {
	
	for (i=0; i<1000; i++) {
		applyGravity(planets)
		applyVelocity(planets)

		printf "LOCATION AFTER ITERATION %d\n", i
		for (planet in planets) {
			planetName = planets[planet]
			print planetName
		}
	}


	total = 0
	for (i in planets) {
		planet = planets[i]
		split(planet, coords, ":")

		pot = abs(coords[1]) + abs(coords[2]) + abs(coords[3])
		kin = abs(coords[4]) + abs(coords[5]) + abs(coords[6])

		total += pot * kin
	}

	printf "Total energy: %s\n", total
}

function abs(v) {
        return v < 0 ? -v : v 
}


function applyGravity(planets, 		difference, handled) {
	for (planetA in planets) {
		planetAName = planets[planetA]
		split(planetAName, aCoords, ":")
		for (planetB in planets) {
			planetBName = planets[planetB]

			if (planetA == planetB) {
				print "planet A and B ar the same"
				continue
			}

			if ((planetA "-" planetB) in handled) {
				printf "%s already handled\n", planetAName "-" planetBName
				continue
			}

			if ((planetB "-" planetA) in handled) {
				printf "%s already handled\n", planetAName "-" planetBName
				continue
			}

			handled[planetA "-" planetB] = 1

			split(planets[planetB], bCoords, ":")

			printf "A coords: %d %d %d\n", aCoords[1], aCoords[2], aCoords[3]
			printf "B coords: %d %d %d\n", bCoords[1], bCoords[2], bCoords[3]
			print ""


			split(difference[planetA], aDiff, ":")
			split(difference[planetB], bDiff, ":")

			for (coordType=1; coordType <= 3; coordType++) {

				cA = aCoords[coordType]
				cB = bCoords[coordType]

				if (cA > cB) {
					aDiff[coordType] -= 1
					bDiff[coordType] += 1
				} 

				if (cB > cA) {
					bDiff[coordType] -= 1
					aDiff[coordType] += 1
				}
			}

			difference[planetA] = aDiff[1] ":" aDiff[2] ":" aDiff[3]
			difference[planetB] = bDiff[1] ":" bDiff[2] ":" bDiff[3]
		}
	}

	for (planet in planets) {
		planetName = planets[planet]
		split(planetName, pCoords, ":")
		
		pDiffString = difference[planet]
		split(pDiffString, pDiff, ":")

		pCoords[4] += pDiff[1]
		pCoords[5] += pDiff[2]
		pCoords[6] += pDiff[3]

		velocity = pCoords[1] ":" pCoords[2] ":" pCoords[3] ":" pCoords[4] ":" pCoords[5] ":" pCoords[6]

		printf "%d velocity: %s\n", planet, velocity
		planets[planet] = velocity
	}
}

function applyVelocity(planets) {
	for (planet in planets) {
		planetName = planets[planet]
		split(planetName, pCoords, ":")

		pCoords[1] += pCoords[4]
		pCoords[2] += pCoords[5]
		pCoords[3] += pCoords[6]

		velocity = pCoords[1] ":" pCoords[2] ":" pCoords[3] ":" pCoords[4] ":" pCoords[5] ":" pCoords[6]

		planets[planet] = velocity
	}
}