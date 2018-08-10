// This problem was asked by Google.
// 
// Implement integer exponentiation. That is, implement the pow(x, y) function, 
// where x and y are integers and returns x^y.
// 
// Do this faster than the naive method of repeated multiplication.
// 
// For example, pow(2, 10) should return 1024.

#include <iostream>
#include <cmath>

int binExp(int x, int n)
{
    if (n == 0)
        return 1;
    else if (n == 1)
        return x;
    else
    {
        auto lg2 = static_cast<int>(std::floor(std::log2(n))); 
        auto residue = n - (1 << lg2);
        auto prev = x;
        for (int i = 0; i < lg2; i++)
            prev *= prev;
        return prev * binExp(x, residue);
    }
}

void test1()
{
    std::cout << "2^10 = " << binExp(2, 10) << std::endl;
}

int main()
{
    test1();
}
