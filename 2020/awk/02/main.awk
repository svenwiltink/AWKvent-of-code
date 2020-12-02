{
   split($1, bounds, "-")
   split($2, char, "")
   split($3, count, char[1])
   if ((length(count) - 1) >= bounds[1] && (length(count) - 1) <= bounds[2] ) {
	   t++
   }
}
END {
	print t
}
