let x = [2, 3, [4, 5]];
println(x[0]);
println(x[2]);

x[1] = 4;
println(x[1]);

println(x.length);
x.append("a");
println(x);
println(x.length);