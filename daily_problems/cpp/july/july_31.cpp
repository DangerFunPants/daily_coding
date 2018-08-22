// Good morning! Here's your coding interview problem for today.
//
// This problem was asked by Facebook.
// 
// Given a function that generates perfectly random numbers between 1 and k 
// (inclusive), where k is an input, write a function that shuffles a deck of 
// cards represented as an array using only swaps.
// 
// It should run in O(N) time.
// 
// Hint: Make sure each one of the 52! permutations of the deck is equally 
// likely.

#include <iostream>
#include <vector>
#include <random>
#include <algorithm>

int random(int k)
{
    std::mt19937 rng;
    rng.seed(std::random_device()());
    std::uniform_int_distribution<std::mt19937::result_type> distk(0,k);
    return distk(rng);
}

std::vector<int> shuffle(const std::vector<int>& cards)
{
    int rem = 52;
    auto res = std::vector<int>(cards);
    for (int& card : res)
    {
        int random_ind = random(rem);
        int select = res.at(random_ind);
        res.at(random_ind) = card;
        card = select;
        --rem;
    }
    return res;
}

void test1()
{
    auto deck = std::vector<int>(52);
    std::iota(deck.begin(), deck.end(), 0);
    auto shuffled = shuffle(deck);
    for (const int& c : shuffled)
    {
        std::cout << c << ", "; 
    }
    std::cout << std::endl;
}

int main()
{
    for (int i = 0; i <= 10; ++i)
    {
        std::cout << "One Shuffle: " << std::endl; 
        test1();
    }
    std::cout << std::endl << std::endl;

    return 0;
}
