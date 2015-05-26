#define CAIDE_STDIN 1
#define CAIDE_STDOUT 1
#include <cstdio>
#include "io.h"

void solve(const CIO& in, const CIO& out);
int main() {
    using namespace std;
#ifdef CAIDE_STDIN
    CIO in(vscanf);
#else
    FileCIO in(CAIDE_IN_FILE, true);
#endif

#ifdef CAIDE_STDOUT
    CIO out(vprintf);
#else
    FileCIO out(CAIDE_OUT_FILE, false);
#endif
    solve(in, out);
    return 0;
}
