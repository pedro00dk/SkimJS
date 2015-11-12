// Array operations
var array1 = [1, 2, 3, 4, 5];
var array2 = [6, 7, 8, 9, 0];

// Native functions
var nativeLen = len(array1);
var nativeHead = head(array1);
var nativeTail = tail(array1);
var nativeConcat = concat(array1, array2);
var nativeCompareTrue = compare(array1, array1);
var nativeCompareFalse = compare(array1, array2);

// JS equivalent native len function
function jsLen(array) {
	var len = 0;
	for (var x in array) len++;
	return len;
}
var javascriptLen = jsLen(array1);