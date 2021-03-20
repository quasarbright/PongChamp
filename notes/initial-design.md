# Compiler Design Process

* High level - what should the language be able to do?
    * syntactically like p5.js
    * runs using a c++ SDL engine (eventually OpenGL)

* Concrete Syntax - what does it look like when you actually write the code?
    * subset of JavasScript supporting the features described below

* Abstract Syntax - How is the AST represented in Haskell
    * Program contains Statements, Statements contains Expressions

## Notes

* Can have two ASTs, for desugaring and interpreting.
* The more you desugar the worse your error messages are.

## Feature List
* Loops
* Datatypes: Number, String, List, Boolean
* Functions : everything in global scope
* Higher order functions (to enhance component funcitonality + easier code)
    - Don't worry about passing to cpp - all components manage their internals themselves engine-side
* Branching
* Maffs and Logics
* Variable assignment
    - Values compatible with C++
* No module system - implicitly import engine


## Example Programs

```pong-champ
init(screenSize)

initX = 0
initY = 0

while True:
    drawRectangle(initX, initY, 20, 20)
    initX = initX + 1
    initY = initY + 1
```

```pong-champ
screenS = 800;
init(screenS, screenS);

initX = 0;
initY = 0;

while (True) {
    drawRectangle(initX, initY, 20, 20);
    initX = initX + 1;
    initY = initY + 1;
    if (initX > screenS) {
        initX = 0;
    }
    if (initY > screenS) {
        initY = 0;
    }
}

```

## Adding a  - template

* High level - what should the feature do?

* Concrete Syntax - 

* Abstract Syntax - 

* What phases does it affect?