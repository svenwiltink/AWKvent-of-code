BEGIN {
    RS="\n\n"
}

function validField(field,      fParts, height, hc, ecl, pid) {
    split(field, fParts, ":")
    if (fParts[1] == "byr") {
        return fParts[2] >= 1920 && fParts[2] <= 2002
    }
    if (fParts[1] == "iyr") {
        return fParts[2] >= 2010 && fParts[2] <= 2020
    }
    if (fParts[1] == "eyr") {
        return fParts[2] >= 2020 && fParts[2] <= 2030
    }
    if (fParts[1] == "hgt") {
        match(fParts[2], "^([0-9]*)([a-z]*)$", height)
        if (height[2] == "cm") {
            return height[1] >= 150 && height[1] <= 193
        }
        return height[1] >= 59 && height[1] <= 76
    }
    if (fParts[1] == "hcl") {
        match(fParts[2], "^#[0-9a-f]{6}$", hc)
        return length(hc) > 0
    }
    if (fParts[1] == "ecl") {
        match(fParts[2], "^(amb)?(blu)?(brn)?(gry)?(grn)?(hzl)?(oth)?$", ecl)
        return length(ecl) > 0
    }
    if (fParts[1] == "pid") {
        match(fParts[2], "^[0-9]{9}$", pid)
        return length(pid) > 0
    }

    return 1
}

NF == 8 {
    t++
    allValid = 1
    for (i=1; i<=NF; i++) {
        split($i, parts, ":")
        if (!validField($i)) {
            allValid = 0
            continue
        }
    }
    allValid ? p2++ : p2 = p2
}

NF == 7 {
    valid = 1
    allValid = 1
    for (i=1; i<=NF; i++) {
        split($i, parts, ":")
        if (parts[1] == "cid") {
            valid = 0
            break
        }

        if (!validField($i)) {
            allValid = 0
            continue
        }
    }

    valid ? t++ : t = t

    if (valid && allValid) {
        p2++
    }
}

END {
    print t
    print p2
}