BEGIN {
	FS = "=>"
}

{
	# recipes is map of target items mapped to the required sources. We store the source items as 
	# 'targetAmount:sourceItem, sourceItem2'. This way we can look up any amount of targetAmount 
	# by just looking up the target item and extracting the actual count from the recipe.

	split($2, targetItem, " ")
	recipes[targetItem[2]] = targetItem[1] ":" $1
}

END {
	part1()
	part2()
}

function part1() {
	oreRequired = getIngredients("1 FUEL", part1Inventory)
	print oreRequired " Ore required"
}

function part2() {
	maxOres = 13312


	max = 1000000000000


	fuelCreated = 1

	while(1) {
		previous = fuelCreated
		cleanArray(part2Inventory)
		
		oreRequired = getIngredients(fuelCreated" FUEL", part2Inventory)
		if (debug) printf "fuel: %d, ore %d\n", fuelCreated, oreRequired

		# determine upper bounds
		if (!reachedCap && oreRequired < max) {
			if (debug) print "not reached max, doubling cap"
			fuelCreated = fuelCreated * 2
			continue
		}

		# overshot. set upper bounds to current
		if (oreRequired > max) {
			reachedCap = 1
			upperBounds = fuelCreated
			if (debug) print "adjusting upper bounds to " upperBounds
		}

		# undershot, set lower bounds to current
		if (oreRequired < max) {
			lowerBounds = fuelCreated
			if (debug) print "adjusting lower bounds to " lowerBounds
		}

		if (debug) printf "%d %d\n", upperBounds, lowerBounds

		fuelCreated = int((upperBounds + lowerBounds) / 2)

		if (previous == fuelCreated) {
			break
		}

	}

	printf "Max amounf of Fuel: %d\n", fuelCreated
}

function cleanArray(array) {
	for (i in array) {
		delete array[i]
	}
}

function getIngredients(targetIngredient, inventory, ident,		targetIngredientInfo, targetItem, timesNeeded, newTargets, newTarget, newOre, oreRequired, remainder) {
	ident++
	split(targetIngredient, targetIngredientInfo, " ")

	targetAmount = targetIngredientInfo[1]
	targetItem = targetIngredientInfo[2]

	if (debug) print "\n"
	if (debug) printf "%*sLooking for item %s. Amount %d\n", ident*2, "", targetItem, targetAmount

	if (targetItem == "ORE") {
		return targetAmount
	}

	recipe = recipes[targetItem]
	split(recipe, items, ":")

	if (debug) printf "%*sfor %d %s we need %s\n", ident*2, "", items[1], targetItem, items[2]

	if (targetItem in inventory) {
		if (debug) printf "%*sWe have %s in inventory\n", ident*2, "", targetItem
		inventoryCount = inventory[targetItem]
		if (inventoryCount >= targetAmount) {
			if (debug) printf "%*sEnough in inventory, no ORE required\n", ident*2, ""
			inventory[targetItem] -= targetAmount
			return 0
		}

		# we don't have enough in inventory, substract the inventory from the required items and try again
		targetAmount -= inventoryCount
		inventory[targetItem] = 0
	}

	# check how many times we need to perform the recipe

	timesNeeded = roundUp(targetAmount / items[1])
	if (debug) printf "%*sNeed to perform the recipe %d times\n", ident*2, "", timesNeeded

	split(items[2], newTargets, ", ")
	oreRequired = 0

	remainder = (timesNeeded * items[1]) - targetAmount
	if (debug) printf "Will have %d %s remaining\n", remainder, targetItem
	
	for (i in newTargets) {
		split(newTargets[i], newTarget, " ")
		newTargetAmount = newTarget[1] * timesNeeded

		if (debug) printf "%*sLooking for child item %d %s\n", ident*2, "", newTargetAmount, newTarget[2]
		
		newOre = getIngredients(newTargetAmount " " newTarget[2], inventory, ident)

		if (debug) printf "%*sNeeded %s ORE for %d %s\n", ident*2, "", newOre, newTargetAmount, newTarget[2]

		oreRequired += newOre
	}

	inventory[targetItem] += remainder
	return oreRequired
}

function roundUp(x) {
	return sprintf("%.0f", (x == int(x)) ? x : int(x)+1)
}
