function fib(x) {
	if (x < 0) throw "Illegal argument";
	if(x == 0 || x == 1) return x;
	return fib(x - 1) + fib(x - 2);
}

// Fib using pd
function fibPd(x) {
	if (x < 0) throw "Illegal argument";
	var obj = {0:0, 1:1};
	for (var i = 2; i <= x; i++) {
		obj[i] = obj[i - 1] + obj[i - 2];
	}
	return obj[x];
}
//fib(30);
fibPd(30);