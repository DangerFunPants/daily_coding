// This problem was asked by Cisco.
// 
// Given an unsigned 8-bit integer, swap its even and odd bits. The 1st and 2nd 
// bit should be swapped, the 3rd and 4th bit should be swapped, and so on.
// 
// For example, 10101010 should be 01010101. 11100010 should be 11010001.
// 
// Bonus: Can you do this in one line?

#include <iostream>
#include <vector>
#include <bitset>

// 0b1010 = 0xA odd bits
// 0b0101 = 0x5 even bits
uint32_t swap_bits(uint32_t n)
{
    return ((0x55555555 & n) << 1 | (0xaaaaaaaa & n) >> 1) | ((n & 0x01) << 1) | ((n & (0x1 << 31)) >> 1);
}

std::string bit_string(uint32_t n, uint32_t bit_size = 8)
{
    return std::bitset<8>(n).to_string();
}

void test1()
{
    std::vector<uint32_t> ts = { 0b10101010
                               , 0b11100010
                               };
    for (auto& t : ts)
    {
        std::cout << "Result for: " << bit_string(t) << " is " << bit_string(swap_bits(t)) << std::endl;
    }
}

int main(int argc, char** argv)
{
    test1();
}
