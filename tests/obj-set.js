let obj = {x: 1, y: {z: 2}};
obj.x = 2;
obj.y.z = 3;
obj.a = 2;
println(obj.x + obj.y.z + obj.a);
