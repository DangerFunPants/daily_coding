#pragma once

#include <iostream>
#include <future> 
#include <functional>

// This problem was asked by Apple.
// 
// Implement a job scheduler which takes in a function f and an integer n, 
// and calls f after n milliseconds.

void run_thread(std::function<void()> func, std::chrono::milliseconds delay)
{
    std::this_thread::sleep_for(delay);
    func();
    return;
}

void run_on_delay(std::function<void()> func, std::chrono::milliseconds delay)
{
    std::async(std::launch::async, run_thread, func, delay);
    return;
}

int main(int argc, char ** argv)
{
    auto f = [](int to_print) { std::cout << "This is my silly little test function with " << to_print << std::endl; };

    std::function<void()> bound = std::bind(f, 5);
    run_on_delay(bound, std::chrono::milliseconds(3000));
    std::cout << "Hola Mundas!" << std::endl;
}