BEGIN {
	total = 0
}

{
   total += int(($1 / 3) - 2)
}

END {
	print total
}