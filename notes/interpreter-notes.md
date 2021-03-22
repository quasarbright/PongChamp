need to:
* fix errors, rung call-engine.js, errors not being thrown
    * maybe check to see if importing c library is causing issue??
    * try running manually in repl to see what interpretProgram returns
* clean code
* finish defining simple engine api
* release PongChamp 0.1.0


Program 
[Function "$0" ["$1"]
    [Function "$2" [] 
        [Assign "$1" (Binop Plus (Var "$1") (Number 1))
        ,Return (Var "$1")]
        ,Return (Var "$2")]
,Let "$3" Nothing
,Assign "$3" (Call (Var "$0") [Number 2])
,Return (Call (Var "$3") [])]


CRISIS

x = 2
function f() {
    return x
}
x = 30
z = 3
y = f()

function g() {
    let x = 234234
    return f()
}





we don't want f to see z, we want f to think x is 30 so it needs that mutation
but you can't just use dynamic scope naively

let top.x = 2
function top.f() {
    let top.f.x = top.x + 1
    return top.f.x
}
top.x = 30
let top.z = 3
let top.y = top.f()

solution: tag everything to uniquify names and then use dynamic scope
also you need to add let to declare

-}

-- State (Map String Cell)

-- top.x = 2
-- function top.f() {
--     let top.f.x = top.x + 1
--     return top.f.x
-- }
-- x = 30
-- z = 3
-- y = f()
