* [x] functions only close over variables free in their body
* [ ] exceptions + try/catch
* [ ] continuations
* [ ] tail calls
* [ ] objects, arrays
* [ ] classes
* [ ] modules/imports
* [ ] algebraic effects?
* [ ] in browser editor and runner with aws ec2 or a lambda
* [ ] check for uninitalized var. not as easy as it seems:
```
let x;
if (...) {
	x = 2;
} else {
	x = 4;
}
let y = x;
```
* [ ] move todo to issues
