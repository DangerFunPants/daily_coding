// This problem was asked by Amazon.
// 
// Given an array of numbers, find the maximum sum of any contiguous subarray 
// of the array.
// 
// For example, given the array [34, -50, 42, 14, -5, 86], the maximum sum 
// would be 137, since we would take elements 42, 14, -5, and 86.
// 
// Given the array [-5, -1, -8, -9], the maximum sum would be 0, since we would 
// not take any elements.
// 
// Do this in O(N) time.

#include <iostream>
#include <vector>
#include <cmath>

int sub_sum(const std::vector<int>& arr)
{
    int sum = 0;
    for (const int& v : arr)
    {
        if ((v < 0) && (std::abs(v) > sum))
            sum = 0;
        else
            sum += v;
    }

    return sum;
}

void test1()
{
    std::vector<int> test_vec1 = { 34, -50, 42, 14, -5, 86 };
    std::vector<int> test_vec2 = { -5, -1, -8, -9 };

    auto res1 = sub_sum(test_vec1);
    std::cout << "res1: " << res1 << std::endl;
    auto res2 = sub_sum(test_vec2);
    std::cout << "res2: " << res2 << std::endl;
}

int main()
{
    test1();
    return 0;
}
