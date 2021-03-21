Right p = parseProgram "" "let x = 2; x = 3; function f(x) {x = x + 4; return x;} return f(x);"

Right p = parseProgram "" "function f(x) { function g() {x = x + 1; return x;} return g;} let g = f(2); return g();"

fmap print (interpretProgram p)