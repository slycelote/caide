template<typename T>
struct TC {
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


#define FOO






    if (*ti.temp() == 'a') return 0;


};





