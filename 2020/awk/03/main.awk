BEGIN {
    FS=""
}

{
   $(((NR - 1) * 1) % NF + 1) == "#" ? sl1++ : sl1=sl1
   $(((NR - 1) * 3) % NF + 1) == "#" ? sl2++ : sl2=sl2
   $(((NR - 1) * 5) % NF + 1) == "#" ? sl3++ : sl3=sl3
   $(((NR - 1) * 7) % NF + 1) == "#" ? sl4++ : sl4=sl4
   $(((NR - 1) / 2) % NF + 1) == "#" && (NR-1) % 2 == 0 ? sl5++ : sl5=sl5
}

END {
    print sl1
    print sl1 * sl2 * sl3 * sl4 * sl5
}