#pragma once
#include <cstdlib>
#include <istream>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <typeinfo>
#include <type_traits>
#include <vector>
#ifndef _MSC_VER
# include <cxxabi.h>
#endif

#if __cplusplus >= 201103L
# include <chrono>
#endif

namespace caide_tester {

template <typename T>
std::string typeName() {
    std::string res = typeid(T).name();
#ifndef _MSC_VER
    auto* p = abi::__cxa_demangle(res.c_str(), 0, 0, 0);
    res = p;
    std::free(p);
#endif

    return res;
}

// Implementation of serializer of structured input/output data (e.g. LeetCode)


template <typename T>
struct Serializer {
    static void writeStructured(std::ostream& out, T val) {
        std::ostringstream os;
        os << __FILE__ << ":" << __LINE__ << " (" << __func__ << "): "
           << "Don't know how to serialize a value of type " << typeName<T>()
           << ".\nHint: unsupported LeetCode type? Consider implementing Serializer<> specialization. "
           << " writeStructured must serialize the value in JSON format, just like in LeetCode samples."
           << " You may want to convert your type to something like std::vector<int> and then use"
           << " the predefined serializer for it.";
        throw std::invalid_argument(os.str());
    }

    static T readPlain(std::istream& in) {
        std::ostringstream os;
        os << __FILE__ << ":" << __LINE__ << " (" << __func__ << "): "
           << "Don't know how to deserialize a value of type " << typeName<T>()
           << ".\nHint: unsupported LeetCode type? Consider implementing Serializer<> specialization. "
           << " readPlain must read the value from a plain text format, where each element is on a separate line,"
           << " and JSON arrays are prefixed by their length on a separate line."
           << " You may want to use a predefined deserializer for something like std::vector<int> and"
           << " then read your type from the vector.";
        throw std::invalid_argument(os.str());
    }
};


template <typename T>
struct NumberSerializer {
    static void writeStructured(std::ostream& out, T val) {
        out << val;
    }
    static T readPlain(std::istream& in) {
        T val;
        if (!(in >> val))
            throw std::invalid_argument(
                    std::string("Couldn't deserialize a value of type ") +
                    typeName<T>());
        std::string eol;
        std::getline(in, eol);
        return val;
    }
};


template <> struct Serializer<int> : NumberSerializer<int> {};
template <> struct Serializer<double> : NumberSerializer<double> {};
template <> struct Serializer<long long> : NumberSerializer<long long> {};


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

extern bool topcoderMode;

template <>
struct Serializer<std::string> {
    static void writeStructured(std::ostream& out, const std::string& val) {
        if (topcoderMode) {
            out << val;
        } else {
            out << '"';
            for (std::size_t i = 0; i < val.size(); ++i) {
                if (val[i] == '\\' || val[i] == '"')
                    out << '\\';
                out << val[i];
            }
            out << '"';
        }
    }

    static std::string readPlain(std::istream& in) {
        std::string line;
        std::getline(in, line);
        return line;
    }
};

template <typename T>
struct Serializer<std::vector<T> > {
    static void writeStructured(std::ostream& out, const std::vector<T>& val) {
        out << (topcoderMode ? '{' : '[');
        for (size_t i = 0; i < val.size(); ++i) {
            if (i > 0)
                out << ',';
            Serializer<T>::writeStructured(out, val[i]);
        }

        out << (topcoderMode ? '}' : ']');
    }

    static std::vector<T> readPlain(std::istream& in) {
        std::size_t length;
        in >> length;
        std::string eol;
        std::getline(in, eol);
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

