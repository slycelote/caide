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

template<class T>
void compilingFunc() {
    // when uncommended, this is invalid syntax, but there should be no error because of delayed template parsing;
};


int main() {
    TC<int> ti;
    ti.temp();
    TC<int*> tip;
    tip.temp();

    func<int>();
    func<int*>();
};
