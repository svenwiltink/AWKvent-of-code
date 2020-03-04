function roundUP(x) {
	return sprintf("%.0f", (x == int(x)) ? x : int(x)+1)
}

{
	print roundUP($1)
}