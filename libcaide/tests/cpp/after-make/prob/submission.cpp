#include <algorithm>
#include <iomanip>
#include <istream>
#include <map>
#include <numeric>
#include <ostream>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
using namespace std;
// Powered by caide (code generator, tester, and library code inliner)


class Solution {
public:
    void solve(std::istream& in, std::ostream& out) {
    }
};

void solve(std::istream& in, std::ostream& out)
{
    out << std::setprecision(12);
    Solution solution;
    solution.solve(in, out);
}
#define CAIDE_STDIN 1
#define CAIDE_STDOUT 1
#include <fstream>
#include <iostream>

#ifdef CAIDE_IN_PATTERN
#if __cplusplus < 201703L
#define CAIDE_STDIN
#undef CAIDE_IN_PATTERN
#else

#include <filesystem>
#include <regex>
#include <stdexcept>

std::string getLatest() {
    using namespace std::filesystem;

    std::string f;
    file_time_type t;

    std::regex regex(CAIDE_IN_PATTERN);

    for (const auto& p : directory_iterator(".")) {
        if (!p.is_regular_file())
            continue;
        auto fileName = p.path().filename().string();
        if (!std::regex_match(fileName, regex))
            continue;
        auto curTime = p.last_write_time();
        if (f.empty() || t < curTime) {
            f = fileName;
            t = curTime;
        }
    }

    if (f.empty())
        throw std::runtime_error("Input file not found");
    std::cerr << "Using " << f << " as input file" << std::endl;
    return f;
}
#endif
#endif

void solve(std::istream& in, std::ostream& out);
int main() {
    using namespace std;
    ios_base::sync_with_stdio(false);
    cin.tie(0);

#if defined(CAIDE_STDIN)
    istream& in = cin;
#elif defined(CAIDE_IN_PATTERN)
    ifstream in(getLatest());
#else
    ifstream in(CAIDE_IN_FILE);
#endif

#ifdef CAIDE_STDOUT
    ostream& out = cout;
#else
    ofstream out(CAIDE_OUT_FILE);
#endif
    solve(in, out);
    return 0;
}
