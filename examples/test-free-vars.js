let x = 1;
let y = 2;
let z = 3;
function f() {
    x = x + 1;
    __printState__();
    let q = 4;
    function g() {
        __printState__();
        y = 7;
        q = q + 1;
    }
    g();
}

f();

