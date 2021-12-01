[
    [[. as $input | to_entries[]|select(.value > $input[.key - 1])] | length][0],
    [[. as $input | length as $len| to_entries[] | select(.key < $len-2) | .value + $input[.key+1] + $input[.key+2]] | [. as $input | to_entries[]|select(.value > $input[.key - 1])] | length][0]
]