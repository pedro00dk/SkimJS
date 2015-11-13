function intMean(array) {
	var mean = 0;
	for (var i = 0; i < len(array); i++) mean += array[i];
	return mean / len(array);
}

var array = [1, 2, 3, 4, 5, 6, 7, 8, 9];

intMean(array);