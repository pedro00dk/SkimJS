function fact(x) {
	if (x < 0) throw "Illegal argument";
	if (x == 0 || x == 1) return 1;
	return x * fact(x - 1);
}

function comb(x, y) {
	if (x < 0 || y < 0) throw "Illegal argument";
	return fact(x) / (fact(y) * fact(x - y));
}
comb(10,3);