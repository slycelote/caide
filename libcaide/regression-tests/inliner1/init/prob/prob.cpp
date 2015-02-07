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

int main() {
    TC<int> ti;
    ti.temp();
    TC<int*> tip;
    tip.temp();

    func<int>();
    func<int*>();

#if 1 > 2
    ti.temp();
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
    if (*ti.temp() == 'a') return 0;
# else
    ti.temp();
# endif
#endif
};

