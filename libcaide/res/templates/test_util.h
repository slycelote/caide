#pragma once
#include <istream>
#include <ostream>
#include <stdexcept>
#include <string>
#include <typeinfo>
#include <vector>

#if __cplusplus >= 201103L
# include <chrono>
#endif

namespace caide_tester {

// Implementation of serializer of structured input/output data
template <typename T>
struct TypeName {
    static const char* name() { return typeid(T).name(); }
};

#define CAIDE_REGISTER_TYPE(X) template <> struct TypeName<X> \
    { static const char* name() { return #X; } }

CAIDE_REGISTER_TYPE(int);
CAIDE_REGISTER_TYPE(double);
CAIDE_REGISTER_TYPE(long long);

template <typename T>
struct Serializer {
    static void writeStructured(std::ostream& out, T val) {
        out << val;
    }
    static T readPlain(std::istream& in) {
        T val;
        if (!(in >> val))
            throw std::invalid_argument(
                    std::string("Couldn't deserialize a value of type ") +
                    TypeName<T>::name());
        std::string eol;
        std::getline(in, eol);
        return val;
    }
};

template <>
struct Serializer<void*> {
    static void writeStructured(std::ostream& out, void*) {
        out << "null";
    }
    static void* readPlain(std::istream& in) {
        return 0;
    }
};

template <>
struct Serializer<bool> {
    static void writeStructured(std::ostream& out, bool val) {
        out << (val ? "true" : "false");
    }

    static bool readPlain(std::istream& in) {
        std::string line;
        std::getline(in, line);

        if (line == "true")
            return true;
        else if (line == "false") {
            return false;
        }

        throw std::invalid_argument(
                std::string("Couldn't deserialize a value of type bool"));
    }
};

template <>
struct Serializer<std::string> {
    static void writeStructured(std::ostream& out, const std::string& val) {
        out << '"' << val << '"';
    }
    static std::string readPlain(std::istream& in) {
        char c;
        in >> c;
        if (c != '"')
            throw std::invalid_argument("Expected a quote character, got " + std::string(1, c));
        std::string s;
        for (;;) {
            c = in.get();
            if (c == '"')
                break;
            if (in.eof())
                throw std::invalid_argument("End of file reached while reading a string");
            s.push_back(c);
        }
        return s;
    }
};

template <typename T>
struct Serializer<std::vector<T> > {
    static void writeStructured(std::ostream& out, const std::vector<T>& val) {
        out << '{';
        for (size_t i = 0; i < val.size(); ++i) {
            if (i > 0)
                out << ',';
            Serializer<T>::writeStructured(out, val[i]);
        }
        out << '}';
    }
    static std::vector<T> readPlain(std::istream& in) {
        std::size_t length;
        in >> length;
        std::string line;
        std::getline(in, line);
        std::vector<T> val;
        val.reserve(length);
        for (std::size_t i = 0; i < length; ++i) {
            val.push_back(Serializer<T>::readPlain(in));
        }
        return val;
    }
};

template <typename T>
void readPlain(std::istream& in, T& val) {
    val = Serializer<T>::readPlain(in);
}

template <typename T>
void writeStructured(std::ostream& out, const T& val) {
    Serializer<T>::writeStructured(out, val);
}

struct Stopwatch {
#if __cplusplus >= 201103L
    using Clock = std::chrono::steady_clock;
    Clock::time_point start;
    Stopwatch(): start(Clock::now()) {}
    std::string GetDuration() const {
        return " #time:" +
            std::to_string(std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count()) +
            "ms";
    }
#else
    std::string GetDuration() const { return ""; }
#endif
};

template <typename T>
void deletePointer(T&) { }

template <typename T>
void deletePointer(T*& p) {
    delete p;
    p = 0;
}

template <>
inline void deletePointer(void*& p) { }

}

