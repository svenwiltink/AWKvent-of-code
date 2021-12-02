$1 == "forward" {x+=$2; y2 += aim * $2}
$1 == "down"    {y1+=$2; aim+=$2}
$1 == "up"      {y1-=$2; aim-=$2}
END { print x * y1; print x * y2}