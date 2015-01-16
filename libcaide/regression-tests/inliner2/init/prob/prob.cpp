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

int main() {
    int x;
    spcppl::f(x);
    goF(X());
    mystd::vector<Vertex> v;
    v.resize(100);
    return 0;
};
