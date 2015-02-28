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

#define DEF_C 'a'
#define DEF_I 0

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






    if (*ti.temp() == DEF_C) return DEF_I;


}

#undef DEF_I




