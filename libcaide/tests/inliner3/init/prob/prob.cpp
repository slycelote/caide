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

int main() {
    f2();
    //f3<int>();
    f4();
    S2 s2;
    f8();
    i4; i8; i10;
}

