let s = 800;

let eng = callEngine(s, s);
clearEngine(eng);

let x = 0;

let stepSize = s / 10;

while (x < s) {
    drawRect(eng, x, x, stepSize, stepSize);
    x = x + stepSize;
}

x = s/2;

while (x > 0) {
    drawRect(eng, x - stepSize, s - x, stepSize, stepSize);
    x = x - stepSize;
}

flipEngine(eng);

while (true) {
    delayEngine(eng, 1);
}