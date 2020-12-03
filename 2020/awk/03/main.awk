BEGIN {
    FS=""
}

{
   $(((NR - 1) * 3) % NF + 1) == "#" ? t++ : t=t
}

END {
    print t
}