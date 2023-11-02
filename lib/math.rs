fn power(x: Int, pow:Int) -> Int
{
    if (x == 0) {
        return 0;
    }
    if (y < 0) {
        return (power(x, y + 1) / x);
    }
    return (power(x, y - 1) * x);
}

fn round(x: Float, y: Int) -> Float
{
    return x;
}

fn abs(x: Int) -> Int
{
    if (x < 0) {
        return (x * -1);
    }
    return x;
}

fn factorial(x: Int) -> Int
{
    if (x < 1) {
        return x * factorial(x - 1);
    }
    return 1;
}

fn sum(x: [Int]) -> Int
{
    res: Int = 0;
    for (i: Int = 0; i < x; i = i + 1) {
        res = res + x[i];
    }
    return x;
}

fn square(x: Int) -> Int
{
    return x;
}

fn isPrime(x: Int) -> Bool
{
    if (n <= 1) {
        return 0;
    }
    if (n == 2 || n == 3) {
        return 1;
    }
    if (n % 2 == 0 || n % 3 == 0) {
        return 0;
    }
    for (int i = 5; i * i <= n; i = i + 6) {
        if (n % i == 0 || n % (i + 2) == 0) {
            return 0;
        }
    }
    return 1;
}

fn log(x: Int) -> Int
{
    return x;
}

fn nlog(x: Int) -> Int
{
    return x;
}

fn exp(x: Int) -> Int
{
    return x;
}

fn cos(x: Int) -> Int
{
    return x;
}

fn sin(x: Int) -> Int
{
    return x;
}

fn tan(x: Int) -> Int
{
    return x;
}

fn acos(x: Int) -> Int
{
    return x;
}

fn asin(x: Int) -> Int
{
    return x;
}

fn atan(x: Int) -> Int
{
    return x;
}

fn degrees(x: Int) -> Int
{
    return x;
}

fn radians(x: Int) -> Int
{
    return x;
}
