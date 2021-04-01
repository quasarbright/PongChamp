let eng = callEngine(300, 300);
println(eng);

let x = 0;
let y = 0;

while (true) {
    clearEngine(eng);
    drawRect(eng, x, y, 40, 40);
    flipEngine(eng);
    x = x + 1;
    y = y + 1;
    if (x > 300) {
        x = 0;
    }
    if (y > 300) {
        y = 0;
    }
    delayEngine(eng, 30);
}