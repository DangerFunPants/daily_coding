// This problem was asked by Google.
// 
// Given the head of a singly linked list, reverse it in-place.

#include <iostream>

template <class T>
struct Node
{
    T _val;
    Node<T>* _next;

    Node(T val, Node<T>* next = nullptr)
        : _val(val), _next(next)
    { }
};

template <class T>
struct LL
{
    Node<T>* _head;

    LL()
        : _head(nullptr)
    { }

    void add_node(T val)
    {
        if (_head == nullptr)
            _head = new Node<T>(val);
        else
        {
            auto p = _head;
            while (p->_next != nullptr)
                p = p->_next;
            p->_next = new Node<T>(val);
        }
    }

    void traverse()
    {
        auto p = _head;
        while (p != nullptr)
        {
            std::cout << p->_val << std::endl;
            p = p->_next;
        }
    }
};

template <class T>
Node<T>* reverse_list(Node<T>* head, Node<T>* prev)
{
    auto n = head->_next;
    if (n == nullptr)
    {
        head->_next = prev;
        return head;
    }
    else
    {
        head->_next = prev;
        return reverse_list(n, head);
    }
}

int main()
{
    auto ll = LL<int>();
    for (int i = 0; i < 10; i++)
        ll.add_node(i);
    ll.traverse();
    ll._head = reverse_list(ll._head, static_cast<Node<int>*>(nullptr));
    ll.traverse();
    return 0;
}
