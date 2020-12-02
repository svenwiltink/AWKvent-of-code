{
   split($1, bounds, "-")
   split($2, char, "")
   split($3, count, char[1])
   split($3, pass, "")
   if ((length(count) - 1) >= bounds[1] && (length(count) - 1) <= bounds[2] ) {
	   p1++
   }

   pass[bounds[1]] == char[1] ? matches = 1 : matches = 0
   pass[bounds[2]] == char[1] ? matches++ : matches = matches

   if (matches == 1) {
	   p2++
   }
}
END {
	print p1
	print p2
}
