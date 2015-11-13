function Stack() {
	return {};
}

function head(stack) {
	if (empty(stack)) throw "Empty stack";
	return stack.head;
}

function tail(stack) {
	if (empty(stack)) throw "Empty stack";
	return stack.tail;
}

function empty(stack) {
	for(var x in stack) return false;
	return true;
}

function size(stack) {
	if (~empty(stack)) return size(tail(stack)) + 1;
	else return 0;
}

function push(stack, val) {
	return {head:val, tail:stack};
}

function peek(stack) {
	return head(stack);
}

function remove(stack) {
	return tail(stack);
}

///////////////////////////////////////////////////////////////////////////////
var stack = Stack();

stack = push(stack, 0);
stack = push(stack, 1);
stack = push(stack, 2);
stack = push(stack, 3);
stack = push(stack, 4);
stack = push(stack, 5);
stack = push(stack, 6);
stack = push(stack, 7);
stack = push(stack, 8);
stack = push(stack, 9);

stack;