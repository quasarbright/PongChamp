# Beginning a space analysis of our interpreter by examining the symtable, stack, and heap

## Recursive calls

Program:
```js
function fac(x) {
	__printState__();
	if(x <= 1) {
		return 1;
	} else {
		return x * fac(x - 1);
	}
}

println(fac(4));
```

STDOUT:
```
symtable:
[("println",0),("print",1),("__printState__",2),("input",3),("str",4),("callEngine",5),("fac",6),("x",7)]
stack:
[(0,CPointer 0),(1,CPointer 1),(2,CPointer 2),(3,CPointer 3),(4,CPointer 4),(5,CPointer 5),(6,CPointer 6),(7,CNumber 4)]
heap:
[(0,<builtin function>),(1,<builtin function>),(2,<builtin function>),(3,<builtin function>),(4,<builtin function>),(5,<builtin function>),(6,<function>: fac fromList [("__printState__",2),("callEngine",5),("fac",6),("input",3),("print",1),("println",0),("str",4)])]

symtable:
[("println",0),("print",1),("__printState__",2),("input",3),("str",4),("callEngine",5),("fac",6),("x",8)]
stack:
[(0,CPointer 0),(1,CPointer 1),(2,CPointer 2),(3,CPointer 3),(4,CPointer 4),(5,CPointer 5),(6,CPointer 6),(7,CNumber 4),(8,CNumber 3)]
heap:
[(0,<builtin function>),(1,<builtin function>),(2,<builtin function>),(3,<builtin function>),(4,<builtin function>),(5,<builtin function>),(6,<function>: fac fromList [("__printState__",2),("callEngine",5),("fac",6),("input",3),("print",1),("println",0),("str",4)])]

symtable:
[("println",0),("print",1),("__printState__",2),("input",3),("str",4),("callEngine",5),("fac",6),("x",9)]
stack:
[(0,CPointer 0),(1,CPointer 1),(2,CPointer 2),(3,CPointer 3),(4,CPointer 4),(5,CPointer 5),(6,CPointer 6),(7,CNumber 4),(8,CNumber 3),(9,CNumber 2)]
heap:
[(0,<builtin function>),(1,<builtin function>),(2,<builtin function>),(3,<builtin function>),(4,<builtin function>),(5,<builtin function>),(6,<function>: fac fromList [("__printState__",2),("callEngine",5),("fac",6),("input",3),("print",1),("println",0),("str",4)])]

symtable:
[("println",0),("print",1),("__printState__",2),("input",3),("str",4),("callEngine",5),("fac",6),("x",10)]
stack:
[(0,CPointer 0),(1,CPointer 1),(2,CPointer 2),(3,CPointer 3),(4,CPointer 4),(5,CPointer 5),(6,CPointer 6),(7,CNumber 4),(8,CNumber 3),(9,CNumber 2),(10,CNumber 1)]
heap:
[(0,<builtin function>),(1,<builtin function>),(2,<builtin function>),(3,<builtin function>),(4,<builtin function>),(5,<builtin function>),(6,<function>: fac fromList [("__printState__",2),("callEngine",5),("fac",6),("input",3),("print",1),("println",0),("str",4)])]

CNumber 24
```

### Analysis
Currently the stack builds up, but can add garbage collection to clear function variabels that are no longer used.