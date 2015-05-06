void f1() {}
void f2() {
    typedef int ii;
    f1();
}

int gen() {
    static int c = 0;
    return ++c;
}

void f4() {
    struct S1 {
        void f5() {
        }
    };
    S1 x;
    static int i = gen();
}


/*
TODO
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
*/

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

int i1, i2 = 0, i3, i4 , i5;

int i7, i8, i9, i10;

#define td int
#define I1 5

td v1[I1], v2;

#define td2 int
#define I2 5

td2 v3[I2];


#define foo int
#define bar foo
void usedFunc() {}
bar y() { return 1; }

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

int f(atd3& a){}

template <typename T>
struct S3 {
    static void fill();
};

template<typename E>
void S3<E>::fill() {
}

template<>
void S3<char>::fill() {
}

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
}

