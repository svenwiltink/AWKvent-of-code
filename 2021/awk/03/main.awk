function str2int(str,   i, parts, result) {
    result = 0

    split(str, parts, "")

    for (i=1; i<= length(parts); i++) {
        result = lshift(result, 1)
        result = or(parts[i], result)
    }

    return result
}

function copyArray(source, target, 		i) {
	for (i in source) {
		target[i] = source[i]
	}
}

BEGIN {
    FS = ""
}

{
    for (i=1; i<=NF; i++) {
        bits[i] += $i
    }

    numbers[NR] = $0
}

END {
    for (i=1; i<=NF; i++) {
        gamma = gamma (bits[i] > NR/2 ? "1" : "0");
        epsilon = epsilon (bits[i] > NR/2 ? "0" : "1");
    }

    gamma = str2int(gamma)
    epsilon = str2int(epsilon)

    print (gamma * epsilon)
}

END {
    i = 1
    copyArray(numbers, part1)
    copyArray(numbers, part2)

    while (length(part1) != 1) {
        for (j=1; j<=NF; j++) {
            bits[j] = 0
        }

        for (j in part1) {
            num = part1[j]
            split(num, parts, "")
            for (p in parts) {
                bits[p] += parts[p]
            }            
        }

        commonbit = bits[i] >= length(part1)/2 ? "1" : "0"

        for (numi in part1) {
            bit = substr(part1[numi], i ,1)
            
            if (bit != commonbit) {
                delete part1[numi]
            }
        }

        i++
    }

    i = 1
    while (length(part2) != 1) {
        for (j=1; j<=NF; j++) {
            bits[j] = 0
        }

        for (j in part2) {
            num = part2[j]
            split(num, parts, "")
            for (p in parts) {
                bits[p] += parts[p]
            }            
        }

        commonbit = bits[i] < length(part2)/2 ? "1" : "0"

        for (numi in part2) {
            bit = substr(part2[numi], i ,1)
            
            if (bit != commonbit) {
                delete part2[numi]
            }
        }

        i++
    }

    for (i in part1) {
        o2 = str2int(part1[i])
    }
    for (i in part2) {
        co2 = str2int(part2[i])
    }

    print o2 * co2
}