#include <cstdio>

template<typename TAG>
struct FileIO {
    static FILE* handle;

    static int scanf(const char* format, ...) {
        int res;
        va_list args;
        va_start(args, format);
        res = vfscanf(handle, format, args);
        va_end(args);
        return res;
    }

    static int printf(const char* format, ...) {
        int res;
        va_list args;
        va_start(args, format);
        res = vfprintf(handle, format, args);
        va_end(args);
        return res;
    }

    ~FileIO() {
        if (handle != 0) {
            fclose(handle);
            handle = 0;
        }
    }
};
#ifndef CAIDE_STDIN
FileIO<1>::handle = 0;
#endif
#ifndef CAIDE_STDOUT
FileIO<2>::handle = 0;
#endif

void solve(int in(const char*...), int out(const char*...));
int main() {
    using namespace std;
#ifdef CAIDE_STDIN
    int (*in)(const char*...) = scanf;
#else
    FileIO<1>::handle = fopen(CAIDE_IN_FILE, "r");
    FileIO<1> instanceIn;
    int (*in)(const char*...) = FileIO<1>::scanf;
#endif

#ifdef CAIDE_STDOUT
    int (*out)(const char*...) = printf;
#else
    FileIO<2>::handle = fopen(CAIDE_OUT_FILE, "w");
    FileIO<2> instanceOut;
    int (*out)(const char*...) = FileIO<2>::scanf;
#endif
    solve(in, out);
    return 0;
}
