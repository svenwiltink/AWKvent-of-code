<?php

$nums = file("input.txt");
$nums = array_map('intval', $nums);
echo count(array_filter($nums, function($value, $key) use ($nums) {
    return $key > 0 && $value > $nums[$key -1];
}, ARRAY_FILTER_USE_BOTH)) . PHP_EOL;

$previous = 0;
$total = 0;
for ($i = 0; $i < count($nums) - 3; $i++) {
    $value = $nums[$i];
    $value += $nums[$i+1];
    $value += $nums[$i+2];

    $total += $i > 0 && $value > $previous ? 1 : 0;
    $previous = $value;
}

echo $total . PHP_EOL;