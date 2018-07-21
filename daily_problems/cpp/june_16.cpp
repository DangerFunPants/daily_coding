// This problem was asked by Google.
//
// An XOR linked list is a more memory efficient doubly linked list. Instead of
// each node holding next and prev fields, it holds a field named both, which is
// an XOR of the next node and the previous node. Implement an XOR linked list; 
// it has an add(element) which adds the element to the end, and a get(index)
//  which returns the node at index.
//
// If using a language that has no pointers (such as Python), you can assume
// you have access to get_pointer and dereference_pointer functions that 
// converts between nodes and memory addresses.

#include <iostream>
#include <random>
#include <ctime>

typedef struct xor_list
{
    int val = 0;
    xor_list * p_node = 0;
} xor_list, * p_xor_list;

p_xor_list bitwise_xor(p_xor_list l1, p_xor_list l2)
{
    auto l1_p = reinterpret_cast<int64_t>(l1);
    auto l2_p = reinterpret_cast<int64_t>(l2);
    return reinterpret_cast<p_xor_list>(l1_p ^ l2_p);
}

void add(p_xor_list& head, int val)
{
    p_xor_list prev = 0;
    p_xor_list trav = head;
    while (trav != 0 && bitwise_xor(prev, trav->p_node) != 0)
    {
        auto key_val = trav->p_node;
        auto next_ptr = bitwise_xor(prev, key_val);
        prev = trav;
        trav = next_ptr;
    }
    // trav now points to the last node in the list
    auto new_node = new xor_list();
    new_node->val = val;
    new_node->p_node = bitwise_xor(trav, 0);
    if (trav == 0)
        head = new_node;
    else
        trav->p_node = bitwise_xor(prev, new_node);
}

int get(p_xor_list head, int index)
{
    p_xor_list prev = 0, trav = head;
    for (int i = 0; i < index; i++)
    {
        auto key_val = trav->p_node;
        auto next_ptr = bitwise_xor(prev, key_val);
        prev = trav;
        trav = next_ptr;
    }

    return trav->val;
}

void traverse(p_xor_list head)
{
    p_xor_list prev = 0, trav = head;
    while (trav != 0)
    {
        std::cout << trav->val << std::endl;
        auto key_val = trav->p_node;
        auto next_ptr = bitwise_xor(prev, key_val);
        prev = trav;
        trav = next_ptr;
    }
}

int main(int argc, char ** argv)
{
    int n = 25;
    std::cout << "adding " << n << " random integers to the XOR list" << std::endl;
    p_xor_list head = 0;
    while (n != 0)
    {
        int rand_int = std::rand();
        std::cout << "Adding: " << rand_int << std::endl;
        add(head, rand_int);
        n--;
    }

    std::cout << "List contents: " << std::endl;
    traverse(head);
}
