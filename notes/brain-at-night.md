so we just realized that scoping and mutable variables is messed up. we're going to uniquify names and interpret with dynamic scope. let will add a new name

wf will check as if it's static scope, but the interpreter will run with dynamic scope

the user will think it's static scope too

we also need to add let and return and stuff



let x = 1

if () {
	let x = 2
	x = 3
}

x = 4



let $1 = 1

if() {
	let $2 = 2
	$2 = 3
}

$1 = 4
