// This problem was asked by Google.
// 
// We can determine how "out of order" an array A is by counting the number of 
// inversions it has. Two elements A[i] and A[j] form an inversion if A[i] > 
// A[j] but i < j. That is, a smaller element appears after a larger element.
// 
// Given an array, count the number of inversions it has. Do this faster than 
// O(N^2) time.
// 
// You may assume each element in the array is distinct.
// 
// For example, a sorted list has zero inversions. The array [2, 4, 1, 3, 5] 
// has three inversions: (2, 1), (4, 1), and (4, 3). The array [5, 4, 3, 2, 1] 
// has ten inversions: every distinct pair forms an inversion.

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <functional>

template <class T>
struct Node
{
    T _elem;
    Node<T>* _left;
    Node<T>* _right;
    size_t _rc_count;

    Node(T elem, Node<T>* left = nullptr, Node<T>* right = nullptr)
        : _elem(elem), _left(left), _right(right)
    { }

    void inc_rc_count()
    { 
        ++_rc_count;
    }

    std::string to_string()
    {
        auto ss = std::stringstream();
        ss << "Elem: " << _elem << " RC: " << _rc_count << std::endl;
        return ss.str();
    }
};
 
template <class T>
int insert(Node<T>** head, T elem)
{
    Node<T>** next_node = nullptr;
    int ret = 0;
    if (*head != nullptr)
    {
        if (elem < (*head)->_elem)
        {
            next_node = &((*head)->_left);
            ret = 1 + (*head)->_rc_count;
        }
        else 
        {
            next_node = &((*head)->_right);
            (*head)->_rc_count++;
        }
    }
    
    if (next_node == nullptr)
    {
        next_node = head;
        *next_node = new Node<T>(elem);
        return ret;
    }
    else
    {
        return ret + insert(next_node, elem);
    }
}

template <class T>
void traverse(Node<T>* head)
{
    if (head == nullptr)
        return;
    else
    {
        std::cout << head->to_string();        
        traverse(head->_left);
        traverse(head->_right);
    }
}

// This method runs in quadratic time. 
int count_inversions(const std::vector<int>& lst)
{
    int invert_count = 0;
    for (size_t i = 0; i < lst.size(); i++)
        for (size_t j = i + 1; j < lst.size(); j++)
        {
            if (lst.at(i) > lst.at(j))
                ++invert_count;
        }
    return invert_count;
}

// This will still end up being quadratic in the worst case
// since the tree implementation is naive and will not
// stay balanced. 
int count_inversions_lg(const std::vector<int>& lst)
{
    int invert_count = 0;
    Node<int>* head = nullptr;
    for (const int& v : lst)
    {
        auto res = insert(&head, v);
        invert_count += res;
    }
    return invert_count;
}

void test1()
{
    std::cout << "TEST 1 RESULTS (Naive Quadratic): " << std::endl;
    const std::vector<int> test1 = { 2, 4, 1, 3, 5 };
    const std::vector<int> test2 = { 5, 4, 3, 2, 1 };
    auto res1 = count_inversions(test1);
    auto res2 = count_inversions(test2);
    std::cout << "res1: " << res1 << std::endl;
    std::cout << "res2: " << res2 << std::endl;
}

void test2()
{
    std::cout << "TEST 2 RESULTS (Heap Based): " << std::endl; 
    const std::vector<int> test1 = { 2, 4, 1, 3, 5 };
    const std::vector<int> test2 = { 5, 4, 3, 2, 1 };
    auto res1 = count_inversions_lg(test1);
    auto res2 = count_inversions_lg(test2);
    std::cout << "res1: " << res1 << std::endl;
    std::cout << "res2: " << res2 << std::endl;
}

int main()
{
    test1();
    test2();
}
