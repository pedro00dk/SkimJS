
///////////////////////////////////////////////////////////////////////////////
// Javascript haskell equivalent List
// The operations cretes new lists, without modify the common

// Returns a empty list
function List() {
	return {}
}

// Returns a list with a new value in the head
function prepend(list, val) {
	return {head: val, tail:list};
}

// Add a element in a specified index, moving the next elements
function addIn(list, val, index) {
	if(index < 0 || index > len(list)) throw "Illegal index";
	if (index != 0) return {head: head(list), tail: addIn(tail(list), val, index - 1)};
	return {head:val, tail:list};
}

// Returns a a list with a new element
function append(list, val) {
	if (~empty(list)) return {head: head(list), tail: append (tail(list), val)};
	return {head:val, tail:{}};
}

// Changes the first element
function setFirst(list, val) {
	if (~empty(list)) return {head: val, tail:tail(list)};
	throw "Empty list"
}

// Change the element of the selected index
function setIn(list, val, index) {
	if(index < 0 || index > len(list)) throw "Illegal index";
	if (index != 0) return {head: head(list), tail: addIn(tail(list), val, index - 1)};
	return {head:val, tail:tail(list)};
}

// Changes the last element
function setLast(list, val) {
	if (~empty(list)) {
		if (~empty(tail(list))) return {head: head(list), tail: append (tail(list), val)};
		return {head:val, tail:{}};
	}
}

// Removes the head element of the list
function remove(list) {
	return tail(list);
}

// Returns the head of the list as an element, if the list is empty throws error
function head(list) {
	if (~empty(list)) return list.head;
	throw "Empty list";
}

// Returns the tail of the list as a new list, if the list is empty throws error
function tail(list) {
	if (~empty(list)) return list.tail;
	throw "Empty list";
}

// Returns if a list is empty
function empty(list) {
	for (x in list) return false;
	return true;
}

// Returns the length of the list
function len(list) {
	if (~empty(list)) return len(tail(list)) + 1;
	return 0;
}

// Return the concatenation of two lists
function concat(list1, list2) {
	if (~empty(list1)) return {head: list1.head, tail: concat(tail(list1), list2)};
	if (~empty(list2)) return {head: list2.head, tail: concat(list1, tail(list2))};
	return {};
}

// Verify if two lists are equals
function equals(list1, list2) {
	if (~empty(list1)) {
		if (~empty(list2)) return equals(tail(list1), tail(list2));
		return false;
	}
	if (~empty(list2)) return false;
	return true;
}
