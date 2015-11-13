var array = [87, 2, 74, 59, 27, 65, 92, 73, 60, 26, 20, 7];

function quicksort() {
	quicksort0(0, len(array) - 1);
}

function quicksort0(beg, end) {
	if (beg >= end) return;

    var pivot = array[(beg + end) / 2];
    var i = beg;
    var j = end;

    while (i <= j) {
        while (i < end && array[i] < pivot) i++;
        while (j > beg && array[j] > pivot) j--;
        if (i <= j) {
            var aux = array[i];
            array[i] = array[j];
            array[j] = aux;
            i++;
            j--;
        }
    }

    if (j > beg) quicksort0(beg, j);
    if (i < end) quicksort0(i, end);
}

quicksort();
array;