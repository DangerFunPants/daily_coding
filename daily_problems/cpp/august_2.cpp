// This problem was asked by Apple.  // 
// Implement a queue using two stacks. Recall that a queue is a FIFO (first-in, 
// first-out) data structure with the following methods: enqueue, which inserts 
// an element into the queue, and dequeue, which removes it.

#include <vector>
#include <iostream>

template <class T>
class Stack
{
private:
    std::vector<T> _elem_store;
public:
    void push(T elem)
    {
        _elem_store.push_back(elem);
    }

    T pop()
    {
        auto last = peek();
        _elem_store.pop_back();
        return last;
    }

    T peek()
    {
        return _elem_store.at(_elem_store.size() - 1);
    }

    bool empty()
    {
        return _elem_store.empty();
    }
};

template <class T>
class Queue
{
private:
    Stack<T> _stack_1; 
    Stack<T> _stack_2; 
public:
    void enqueue(T elem)
    {
        _stack_1.push(elem);
    }

    T dequeue()
    {
        while (!_stack_1.empty())
            _stack_2.push(_stack_1.pop());
        T ret = _stack_2.pop();
        while(!_stack_2.empty())
            _stack_1.push(_stack_2.pop());
        return ret;
    }

    bool empty()
    {
        return _stack_1.empty();
    }
};

void queue_test()
{
    auto q = Queue<int>();
    for (int i = 0; i < 10; i++)
        q.enqueue(i);
    for (int i = 0; i < 5; i++)
    {
        auto v = q.dequeue();
        std::cout << v << std::endl;
    }
    for (int i = 10; i < 15; i++)
        q.enqueue(i);
    while (!q.empty())
    {
        auto v = q.dequeue();
        std::cout << v << std::endl;
    }
}

int main()
{
    queue_test();
    return 0;
}
