#include <fstream>
#include <iostream>

void solve(std::istream& in, std::ostream& out);
int main() {
    using namespace std;
    ios_base::sync_with_stdio(false);
    cin.tie(0);

#ifdef CAIDE_STDIN
    istream& in = cin;
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
