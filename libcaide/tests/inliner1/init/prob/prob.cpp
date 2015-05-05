template<typename T>
struct TC {
    const char* temp() {
        return "TC";
    }
};

template<typename T>
struct TC<T*> {
    const char* temp() {
        return "TC<T*>";
    }
};

template<>
struct TC<int> {
    const char* temp() {
        return "TC<int>";
    }
};

template<typename T>
const char* func() { return "T"; }

template<>
const char* func<int>() {return "int";}

template<>
const char* func<int*>() { return "int*"; }

#define MULTILINE \
    f(); \
    g();

#define DEF_C 'a'
#define DEF_I 0
#define DEF_I2 2
#define DEF_I3 3

int unused_func() {
    return DEF_I3;
}

template<typename T>
class TemplateMethodInTemplateClass {
public:
    template<typename It>
    TemplateMethodInTemplateClass(It a, It b) {}

    template<typename It>
    void print(It a) {}
};

int main() {
    TC<int> ti;
    ti.temp();
    TC<int*> tip;
    tip.temp();

    func<int>();
    func<int*>();

    if (true)
        ;

    TemplateMethodInTemplateClass<double> c(1, 2);
    c.print(3);

#if 1 > 2
    ti.temp();
    return DEF_I2;
#elif 1 > 3
    tip.temp();
#elif defined(FOO)
    func<int>();
#endif

#define FOO

#ifndef FOO
    ti.temp();
#endif

#ifdef BAR
    ti.temp();
#else
# ifdef FOO
    if (*ti.temp() == DEF_C) return DEF_I;
# else
    ti.temp();
# endif
#endif

#ifdef ONLINE_JUDGE
    ti.temp();
#else
    tip.temp();
#endif

#if 1 < 2
    ti.temp();
#elif defined(ONLINE_JUDGE)
    tip.temp();
#endif
};
#undef DEF_I

