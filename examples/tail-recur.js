function fac_help(x, acc) {
    __printState__();
	if(x <= 1) {
		return acc;
	} else {
		return fac_help(x - 1, x * acc);
	}
}

function fac(x) {
    return fac_help(x, 1);
}

print(fac(4));