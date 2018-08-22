// This problem was asked by Microsoft.
// 
// Implement a URL shortener with the following methods:
// 
//     shorten(url), which shortens the url into a six-character alphanumeric 
// string, such as zLg6wl.
//     restore(short), which expands the shortened string into the original 
// url. If no such shortened string exists, return null.
// 
// Hint: What if we enter the same URL twice?

#include <iostream> 
#include <string>
#include <functional>
#include <vector>
#include <cmath>
#include <sstream>
#include <random>
#include <chrono>
#include <map>

std::map<std::string, std::string> g_url_map;

std::string shorten(std::vector<char> digs, const std::string& url)
{
    auto seed = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::system_clock::now().time_since_epoch()).count();
    auto rng = std::default_random_engine(seed);
    auto uni_dist = std::uniform_int_distribution<size_t>();
    size_t salt = uni_dist(rng);
    auto salted = std::to_string(salt) + url;
    
    size_t hash_val = std::hash<std::string>{}(salted);
    
    size_t mod_val = hash_val % static_cast<size_t>((std::pow(62, 6))); 
    auto url_digs = std::vector<int>(6);
    for (int i = 5; i >= 0; --i)
    {
        auto res = mod_val / static_cast<int>(std::pow(62, i));
        mod_val = mod_val - (res * static_cast<int>(std::pow(62, i)));
        url_digs.at(5 - i) = digs.at(res);
    }

    auto ss = std::stringstream();
    for (const char& d : url_digs)
        ss << d; 

    std::string short_str = ss.str();
    g_url_map[short_str] = url;
    return short_str;
}

std::string restore(const std::string& shortened)
{
    return g_url_map[shortened];
}

void short_test()
{
    auto digs = std::vector<char>();
    for (int i = 0; i < 26; i++)
        digs.push_back(i + 65);
    for (int i = 0; i < 26; i++)
        digs.push_back(i + 97);
    for (int i = 0; i < 10; i++)
        digs.push_back(i + 48);

    std::vector<std::string> urls = { std::string("www.google.ca")
                                    , std::string("www.amazon.com")
                                    , std::string("www.facebook.com")
                                    , std::string("www.reddit.com")
                                    };
    for (const std::string& str : urls)
    {
        std::cout << "Original: " << str << std::endl;
        auto short_url = shorten(digs, std::string(str));
        std::cout << "Shortened: " << short_url << std::endl;
        auto res = restore(short_url);
        std::cout << "Restored: " << res << std::endl;
    }
}

int main()
{
    short_test();
    return 0;
}
