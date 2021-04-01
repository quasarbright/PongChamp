function fac(x) {
	__printState__();
	if(x <= 1) {
		return 1;
	} else {
		return x * fac(x - 1);
	}
}

println(fac(4));
