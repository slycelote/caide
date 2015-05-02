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

int main() {
    f2();
    //f3<int>();
    f4();
}

