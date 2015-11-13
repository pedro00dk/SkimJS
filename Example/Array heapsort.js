var array = [87, 2, 74, 59, 27, 65, 66, 9, 7, 10, 11];
function heapsort() {
    for (var i = len(array) - 1; i >= 0; i--) heapfy(i, len(array) - 1);

    for (var i = len(array) - 1; i > 0; i--) {
        var aux = array[i];
        array[i] = array[0];
        array[0] = aux;
        heapfy(0, i - 1);
    }
}

function heapfy(elo, end) {
    var father = elo;
    var son1 = (father * 2) + 1;
    var son2 = (father * 2) + 2;
    var bigger;

    while (son1 <= end) {
        if (son2 <= end && array[son1] < array[son2]) bigger = son2;
        else bigger = son1;

        if (array[father] < array[bigger]) {
            var aux = array[father];
            array[father] = array[bigger];
            array[bigger] = aux;
            father = bigger
            son1 = (father * 2) + 1;
            son2 = (father * 2) + 2;
        } else break;
    }
}

heapsort();
array;