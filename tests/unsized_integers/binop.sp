// COMPILER OUTPUT 11111111
const foo = fn () => {
    if
    {3 +  2} == 5 &&
    {3 -  2} == 1 &&
    {3 *  2} == 6 &&
    {3 /  2} == 1 &&
    {3 %  2} == 1 &&
    {3 ^  2} == 1 &&
    {3 &  2} == 2 &&
    {3 |  2} == 3 &&
    {3 << 2} == 12 &&
    {3 >> 2} == 0 &&
    {3 == 2} == false &&
    {3 != 2} == true &&
    {3 <  2} == false &&
    {3 >  2} == true &&
    {3 <= 2} == false &&
    {3 >= 2} == true
    then
    1111
    else
    2222
}
const bar = fn () => {
    let _0 = 0;
    let _1 = 1;
    let _2 = 2;
    let _3 = 3;
    let _4 = 4;
    let _5 = 5;
    let _6 = 6;
    let _12 = 12;
    if
    {_3 +  _2} == _5 &&
    {_3 -  _2} == _1 &&
    {_3 *  _2} == _6 &&
    {_3 /  _2} == _1 &&
    {_3 %  _2} == _1 &&
    {_3 ^  _2} == _1 &&
    {_3 &  _2} == _2 &&
    {_3 |  _2} == _3 &&
    {_3 << _2} == _12 &&
    {_3 >> _2} == _0 &&
    {_3 == _2} == false &&
    {_3 != _2} == true &&
    {_3 <  _2} == false &&
    {_3 >  _2} == true &&
    {_3 <= _2} == false &&
    {_3 >= _2} == true
    then
    1111
    else
    2222
}

const main = fn () => foo() * 10000 + bar()