label $out |
. as $input |
        $input[] as $n1 |
        $input[] as $n2 |
        select($n1 + $n2 == 2020) |
        $n1 * $n2, break $out