BEGIN {
	RS="[0-9]{150}"
}

{
	if (NR == 1) {
		image = RT
	}

	split(image, currentImage, "")
	split(RT, newLayer, "")

	newImage = ""
	for (i in currentImage) {
		if (currentImage[i] == 2) {
			currentImage[i] = newLayer[i]
		}

		newImage = newImage currentImage[i]
	}

	image = newImage
}

END {
	split(image, characters, "")
	for (j in characters) {
		printf characters[j] == 1 ? "#" : "."

		if (j%25 == 0) {
			print " "
		}
	}
}