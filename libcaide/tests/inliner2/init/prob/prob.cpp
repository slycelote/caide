#include <myvector>

namespace spcppl {

    template <typename T>
    double f(T& t) {
        return double(t);
    }

} // namespace spcppl

class X {
public:
    void f() const {
    }
};

template <typename T>
void goF(const T& t) {
    t.f();
}

class Vertex {
public:
    Vertex() {}
    Vertex(int a, int b) {}
};

struct Y {
    bool operator ++ (int x) {
        return true;
    }
    int operator() (int x) {
        return 0;
    }
};

bool operator + (Y a, Y b) {
    return true;
}

struct Used {
    int x;

    ~Used() {
        x = 2;
    }
};

struct Unused {
    int x;
    ~Unused() {
        x = 3;
    }
};

struct UsedTypedefedStruct {
    int a;
};

typedef UsedTypedefedStruct UsedTypedef;

struct UnusedTypedefedStruct {
    int a;
};

typedef UnusedTypedefedStruct UnusedTypedef;
typedef UsedTypedefedStruct UnusedTypedef2;

struct UsedTypedefedStruct2 {
    int a;
};

typedef UsedTypedefedStruct2 UsedTypedef2;
typedef UsedTypedef2 RecursiveUsedTypedef2;

void unused_global_func() {}

struct UsedCall {
    static void staticCall() {}
};

template<typename T>
struct V {
    T t;
};

template<typename T>
struct V2 {
    T t;
};


template<typename T>
struct V3 {
    T t;
};


struct S1 {
    typedef int type;
};

template<typename T>
struct S2 {
    typedef T type;
};

template<typename T>
struct S3 {
    typedef T type;
    static type get() {
        return type();
    }
};

struct S4 {
    typedef V2<int> type;
};

struct S5 {
    typedef V3<int> type;
};

template<typename T>
struct S6 {
};

struct S7 {};

template<int n>
struct IntTemplate {
    static const int val = n*n;
};

typedef long long ll;

#include "inc.h"

int main() {
    {
        int x;
        spcppl::f(x);
        goF(X());
    }

    {
        mystd::vector<Vertex> v;
        v.resize(100);
    }

    {
        Y y;
        y++;
        y + y;
    }

    {
        Used used;
    }

    {
        UsedTypedef f;
        RecursiveUsedTypedef2 g;
    }

    {
        mystd::vector<UsedCall>::callIntoTemplate();
    }

    {
        S1::type i;
        auto v = S3<V<int> >::get();
        S4::type v2;
    }

    {
        S6<ll> s6;
        S6<S7> s6_s7;
    }

    {
        IntTemplate<4>::val;
    }

    {
        typedef int ta;
        typedef int tb;
        ta i = 2;
        struct LocalUsed {
            typedef int tc;
            void used() {}
            void unused() {}
        } lu;
        lu.used();
        struct LocalUnused {};
    }

    {
        struct Outer {
            struct Inner {};
        };
        Outer::Inner inner;
    }

    return 0;
}

