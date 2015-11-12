function mulMatrices(x, y, dim) {
	var result = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
	for(var i = 0; i < dim; i++) for (var j = 0; j < dim; j++) for(var k = 0; k < dim; k++)
			result[(i * dim) + j] += x[(i * dim) + k] * y[(k * dim) + j];
	return result;
}

var m1 = [1, 0, 0, 1,
		  0, 1, 0, 3,
		  0, 0, 1, 2,
		  0, 0, 0, 1];
		  
var m2 = [1, 0, 0, -6,
		  0, 1, 0, -2,
		  0, 0, 1, 2,
		  0, 0, 0, 1];

mulMatrices(m1, m2, 4);