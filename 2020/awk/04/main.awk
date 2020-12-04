BEGIN {
    RS="\n\n"
}

NF == 8 {t++}
NF == 7 {
    valid = 1
    for (i=1; i<=NF; i++) {
        split($i, parts, ":")
        if (parts[1] == "cid") {
            valid = 0
            break
        }
    }

    valid ? t++ : t = t
}

END {
    print t
}