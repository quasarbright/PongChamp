function f(x) {
	print("inside");
	__printState__();
	return x;
}
__printState__();
return f(1890);
