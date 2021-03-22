function f(x) {
	function g() {
		x = x + 1;
		return x;
	}
	__printState__();
	return g;
}

let y = 77;
let h = f(2);
h();
return h();
// stack gets fucked in call. symtable is fine
