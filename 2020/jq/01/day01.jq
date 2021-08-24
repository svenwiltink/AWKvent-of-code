. as $input |
[
(label $part1 |
        $input[] as $n1 |
        $input[] as $n2 |
        select($n1 + $n2 == 2020) |
        $n1 * $n2, break $part1),
(label $part2 |
        $input[] as $n1 |
        $input[] as $n2 |
        $input[] as $n3 |
        select($n1 + $n2 + $n3 == 2020) |
        $n1 * $n2 * $n3, break $part2)
] | .[]