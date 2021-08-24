[ . | split("\n") | .[] | capture("(?<min>[0-9]+)-(?<max>[0-9]+) (?<letter>[a-z]{1}): (?<password>[a-z]+)") |
    . as $entry |
        .password | split("") | [.[] | select( . == $entry.letter )] | length as $num |
    $entry | select($num >= (.min|tonumber) and $num <= (.max|tonumber))
] | length