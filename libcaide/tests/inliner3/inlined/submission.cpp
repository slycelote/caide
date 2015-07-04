void f1() {}
void f2() {
    f1();
}

int gen() {
    static int c = 0;
    return ++c;
}

void f4() {
    struct S1 {
    };
    S1 x;
    static int i = gen();
}


template<typename T>
struct Outer
{
    struct Inner
    {
    };

    Inner inner;
};

template<typename T>
void f3() {
    Outer<T> v;
    v.inner;
}

int f6() {
    return 6;
}

struct S2 {
    int i;
    S2()
        : i(f6())
    {}
};

int f7() {
    return 7;
}

void f8(int i = f7()) {
}

/// caide keep
void f9() {}

int i4 ;

int i8, i10;

#define td int

td v2;

void usedFunc() {}

typedef double db;
db dp[100];

struct A {
    static const int x = 2;
};
typedef A VI;
struct B : VI {
};

typedef double DD;

typedef A atd1;
typedef A atd2;
typedef A atd3;
typedef A atd4;

void f(atd3& a){}

struct S4 {
    S4(int a, int b){}
};

typedef S4 tds4;

template<typename T>
struct Identity { typedef T type; };

typedef Identity<int>::type inttd;

template<typename T>
void noopFunc(T t) {}

template<typename T>
void forwaredDeclared();

class DefaultTypeParam {};

template<typename T=DefaultTypeParam>
class WithDefaultTypeParam
{
};

int main() {
    f2();
    //f3<int>();
    f4();
    S2 s2;
    f8();
    i4 = i8 = i10 = 1;
    usedFunc();
    v2 = 1;
    dp[0] = 1;
    B b;
    dp[0] = (DD)1;
    atd1* ptr = 0;
    new atd2[10];
    f(b);
    int i = atd4::x;
    tds4(1, 2);
    inttd j;
    {
        typedef int Int;
        noopFunc([&](Int& i){});
    }
    {
        f3<char>();
    }
    forwaredDeclared<int>();
    WithDefaultTypeParam<int> w;
}

template<typename T>
void forwaredDeclared() {
}

