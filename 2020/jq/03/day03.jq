[
    . | split("\n") | map(split("")) | . as $slope |
    $slope[0] | length as $width |
    $slope | to_entries | .[] |
        select($slope[.key][(3 * .key) % $width] == "#")
] | length