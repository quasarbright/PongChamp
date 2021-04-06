function fac(x) {
	if(x <= 1) {
		throw x;
	} else {
        let result;
        try {
            result = fac(x - 1);
        }
        catch (e) {
            result = e;
        }
        return result * x;
	}
}

println(fac(4));