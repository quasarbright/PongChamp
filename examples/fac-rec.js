function fac(x) {
	// print(x);
	if(x <= 1) {
		// print("iii");
		return 1;
	} else {
		print(fac(x-1));
		let ans = x * fac(x - 1);
		// print(ans);
		return ans;
	}
}

println(fac(4));
