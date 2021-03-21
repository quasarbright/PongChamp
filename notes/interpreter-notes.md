
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
