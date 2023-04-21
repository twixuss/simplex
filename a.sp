const main = () => {
    var a: U16 = 1;
    var b: U16 = 2;
    var x = if true a else b
    println(x)
}

const println = (value: S64) => #intrinsic