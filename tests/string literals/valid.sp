const main = () => {
    println("Hello\a\b\f\v\n\t\r\x00\xff\x0f")
}

const println = (value: String) => #intrinsic