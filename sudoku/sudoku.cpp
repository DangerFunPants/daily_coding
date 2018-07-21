
#include <vector>
#include <stdint.h>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <set>

class SudokuBoard
{
private:
    std::vector<std::vector<uint16_t>> _board;

    std::vector<uint16_t> _marks = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    uint16_t get_mk(size_t i, size_t j)
    {
        return _board.at(i).at(j);
    }

    void set_mk(size_t i, size_t j, uint16_t mk)
    {
        _board.at(i).at(j) = mk;
    }

    std::vector<uint16_t> get_valid_for_row(size_t i)
    {
        auto row = _board.at(i);
        std::sort(row.begin(), row.end());
        auto res = std::vector<uint16_t>();
        std::set_difference(_marks.begin(), _marks.end(), row.begin(), row.end(), std::back_inserter(res));
        return res;        
    }

    std::vector<uint16_t> get_valid_for_col(size_t j)
    {
        auto col = std::vector<uint16_t>();
        std::transform(_board.begin(), _board.end(), std::back_inserter(col), 
            [&j] (const auto& v)
            {
                return v.at(j);
            });
        std::sort(col.begin(), col.end());
        auto res = std::vector<uint16_t>();
        std::set_difference(_marks.begin(), _marks.end(), col.begin(), col.end(), std::back_inserter(res));
        return res;
    }

    std::vector<uint16_t> get_valid_for_box(size_t i, size_t j)
    {
        size_t row_begin = (i / 3) * 3, row_end = row_begin + 3;
        size_t col_begin = (j / 3) * 3, col_end = col_begin + 3;
        auto box = std::vector<uint16_t>();
        for (auto i = row_begin; i < row_end; i++)
            for (auto j = col_begin; j < col_end; j++)
                box.push_back(get_mk(i, j));
        std::sort(box.begin(), box.end());
        auto res = std::vector<uint16_t>();
        std::set_difference(_marks.begin(), _marks.end(), box.begin(), box.end(), std::back_inserter(res));
        return res;
    }

    std::vector<uint16_t> get_valid_mks(size_t i, size_t j)
    {
        auto r = get_valid_for_row(i);
        auto c = get_valid_for_col(j);
        auto b = get_valid_for_box(i, j);
        auto res = std::vector<uint16_t>();
        auto res1 = std::vector<uint16_t>();
        std::set_intersection(r.begin(), r.end(), c.begin(), c.end(), std::back_inserter(res));
        std::set_intersection(res.begin(), res.end(), b.begin(), b.end(), std::back_inserter(res1));
        return res1;
    }

    std::pair<uint16_t, uint16_t> get_next(const std::pair<uint16_t, uint16_t> ij)
    {
        if (ij.first + 1 >= _board.size())
            return std::make_pair(0, ij.second + 1);
        else
            return std::make_pair(ij.first + 1, ij.second);
    }

public:
    SudokuBoard(const std::vector<std::vector<uint16_t>> & board)
        : _board(board)
    { }

    bool solve(std::pair<uint16_t, uint16_t> ij)
    {
        if (ij.second == 9)
        {
            for (auto & r : _board)
            {
                for (auto & c : r)
                {
                    std::cout << c << " ";
                }
                std::cout << std::endl;
            }
            return true;
        }

        auto valid_mks = get_valid_mks(ij.first, ij.second);
        if (get_mk(ij.first, ij.second) == 0)
        {
            if (valid_mks.empty())
            {
                return false;
            }
            else
            {
                for (auto & mk : valid_mks)
                {
                    set_mk(ij.first, ij.second, mk);
                    auto v = solve(get_next(ij));
                    set_mk(ij.first, ij.second, 0);
                }
                return true;
            }
        }
        else
        {
            return solve(get_next(ij));
        }
    }
};

int main(int argc, char ** argv)
{
    std::vector<std::vector<uint16_t>> puzzle = 
            { { 5, 3, 0, 0, 7, 0, 0, 0, 0 }
            , { 6, 0, 0, 1, 9, 5, 0, 0, 0 }
            , { 0, 9, 8, 0, 0, 0, 0, 6, 0 }
            , { 8, 0, 0, 0, 6, 0, 0, 0, 3 }
            , { 4, 0, 0, 8, 0, 3, 0, 0, 1 }
            , { 7, 0, 0, 0, 2, 0, 0, 0, 6 }
            , { 0, 6, 0, 0, 0, 0, 2, 8, 0 }
            , { 0, 0, 0, 4, 1, 9, 0, 0, 5 }
            , { 0, 0, 0, 0, 8, 0, 0, 7, 9 }
            };
    auto p = SudokuBoard(puzzle);
    p.solve(std::make_pair(0, 0));
}