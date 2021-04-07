function f(x) {
	function g(y) {
		return x + y;
	}
	return g;
}
println(f(3)(5));
