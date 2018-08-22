// This problem was asked by Google.
// 
// Implement an LRU (Least Recently Used) cache. It should be able to be // initialized with a cache size n, and contain the following methods: // 
//     set(key, value): sets key to value. If there are already n items in the 
// cache and we are adding a new item, then it should also remove the least 
// recently used item.
//     get(key): gets the value at key. If no such key exists, return null.
// 
// Each operation should run in O(1) time.

#include <map>
#include <iostream> 
#include <string>
#include <sstream>

template <class T>
struct Node
{
    T _val;
    Node<T>* _next;
    Node<T>* _prev;

    Node(T val, Node* next = nullptr, Node* prev = nullptr)
        : _val(val), _next(next), _prev(prev)
    { } 
};

template <class T>
struct DLL
{
private:
    Node<T>* _head;
    Node<T>* _tail;
    size_t _size; 
public:
    DLL()
        : _head(nullptr), _tail(nullptr), _size(0)
    { } 

    ~DLL()
    {
        auto to_pop = _size;
        for (size_t i = 0; i < to_pop; i++)
            pop_back();
        _head = nullptr;
        _tail = nullptr;
    }

    void push_front(T elem)
    {
        ++_size;
        auto new_node = new Node<T>(elem, _head);
        if (_head != nullptr)
            _head->_prev = new_node;
        else
            _tail = new_node;
        _head = new_node;
    }

    void pop_back()
    {
        --_size;
        Node<T>* p = _tail->_prev;
        if (p != nullptr)
            p->_next = nullptr;
        delete _tail;
        _tail = p;
    }

    void del_node(Node<T>* p_node)
    {
        Node<T>* prev = p_node->_prev;
        Node<T>* next = p_node->_next;

        if (p_node != _head)
            prev->_next = next;
        else
            _head = p_node->_next;

        if (p_node != _tail)
            next->_prev = prev;
        else
            _tail = p_node->_prev;

        --_size;
        delete p_node;
    }

    void traverse()
    {
        for (auto p = _head; p != nullptr; )
        {
            std::cout << p->_val << std::endl;
            p = p->_next;
        }
    }

    std::string to_string()
    {
        auto ss = std::stringstream();
        Node<T>* p = _head;
        for (; p->_next != nullptr; )
        {
            ss << p->_val << " -> ";
            p = p->_next;
        }
        ss << p->_val << std::endl;
        return ss.str();
    }

    Node<T>* at(size_t index)
    {
        Node<T>* p_head = _head;
        for (int i = 0; i < index; i++)
            if (p_head != nullptr)
                p_head = p_head->_next;
        return p_head;
    }

    Node<T>* front()
    {
        return _head;
    }

    Node<T>* back()
    {
        return _tail;
    }

    size_t size()
    {
        return _size;
    }
};

template <class K, class V>
class LRUCache
{
private:
    std::map<K, std::pair<Node<K>*, V>> _kv_store; 
    DLL<K> _ll; 
    size_t _cache_size;
    
public:
    LRUCache(size_t cache_size)
        : _kv_store(), _cache_size(cache_size), _ll()
    { }

    void set(K key, V val)
    {
        auto iter = _kv_store.find(key);
        if (iter != _kv_store.end())
        {
            auto kvp = std::get<1>(*iter);
            auto p_node = std::get<0>(kvp);
            _ll.del_node(p_node);
            _ll.push_front(key);
            _kv_store[key] = std::make_pair(_ll.front(), val);
        }
        else
        {
            _ll.push_front(key);
            _kv_store[key] = std::make_pair(_ll.front(), val);
            if (_kv_store.size() > _cache_size)
            {
                auto to_rem = _ll.back();
                _kv_store.erase(to_rem->_val);
                _ll.pop_back();
            }
        }
    }

    V get(K key)
    {
        auto res = _kv_store.at(key);
        _ll.del_node(std::get<0>(res));
        _ll.push_front(key);
        return std::get<1>(res);
    }

    std::string to_string()
    {
        auto ss = std::stringstream();
        ss << "Queue: " << std::endl;
        ss << _ll.to_string() << std::endl;
        ss << "Map: " << std::endl;
        for (auto iter = _kv_store.begin(); iter != _kv_store.end(); iter++)
        {
            auto kvp = std::get<1>(*iter);
            ss << "Key: " << std::get<0>(*iter) << " Val: " << std::get<1>(kvp) << std::endl;
        }
        return ss.str();
    }

};

void lru_cache_test()
{
    auto lru = LRUCache<int, std::string>(4);
    lru.set(1, std::string("hola mundas"));
    lru.set(2, std::string("hola amigos"));
    lru.get(1);

    lru.set(3, std::string("hello world"));
    lru.set(4, std::string("hello friends"));
    lru.set(5, std::string("hello again"));

    std::cout << lru.to_string();
}

void dll_test()
{
    auto ll = DLL<int>();
    for(int i = 0; i < 10; i++)
    {
        ll.push_front(i);
    }
    ll.traverse();

    for (int i = 5; i < 10; i++)
    {
        auto p_node = ll.at(5);
        std::cout << "Node: " << p_node->_val << std::endl;
        ll.del_node(p_node);
    }
        
    std::cout << ll.at(0)->_val << std::endl;
    ll.del_node(ll.at(0));
    ll.del_node(ll.at(ll.size() - 1));

    ll.traverse();
}

int main()
{
    lru_cache_test();
    return 0;
}

