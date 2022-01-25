#pragma once
#include <istream>
#include <ostream>
#include <stdexcept>
#include <string>
#include <typeinfo>
#include <vector>


namespace caide_tc {

// Implementation of serializer of topcoder input/output data
template<typename T>
struct TypeName {
    static const char* name() { return typeid(T).name(); }
};

#define CAIDE_REGISTER_TYPE(X) template<> struct TypeName<X> \
    { static const char* name() { return #X; } }

CAIDE_REGISTER_TYPE(int);
CAIDE_REGISTER_TYPE(double);
CAIDE_REGISTER_TYPE(long long);

template<typename T>
struct Serializer {
    static void serialize(std::ostream& out, T val) {
        out << val;
    }
    static T deserialize(std::istream& in) {
        T val;
        if (!(in >> val))
            throw std::invalid_argument(
                    std::string("Couldn't deserialize a value of type ") +
                    TypeName<T>::name());
        return val;
    }
};

template<>
struct Serializer<bool> {
    static void serialize(std::ostream& out, bool val) {
        out << (val ? "true" : "false");
    }

    static bool deserialize(std::istream& in) {
        std::string val(4, ' ');
        for (int i = 0; i < 4; ++i)
            if (!(in >> val[i]))
                throw std::invalid_argument(
                        std::string("Couldn't deserialize a value of type bool"));

        if (val == "true")
            return true;
        else if (val == "fals") {
            char c;
            in >> c;
            if (c == 'e')
                return false;
        }

        throw std::invalid_argument(
                std::string("Couldn't deserialize a value of type bool"));
    }
};

template<>
struct Serializer<std::string> {
    static void serialize(std::ostream& out, const std::string& val) {
        out << '"' << val << '"';
    }
    static std::string deserialize(std::istream& in) {
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

template<typename T>
struct Serializer<std::vector<T> > {
    static void serialize(std::ostream& out, const std::vector<T>& val) {
        out << '{';
        for (size_t i = 0; i < val.size(); ++i) {
            if (i > 0)
                out << ',';
            Serializer<T>::serialize(out, val[i]);
        }
        out << '}';
    }
    static std::vector<T> deserialize(std::istream& in) {
        char c;
        in >> c;
        if (c != '{' && c != '[') // Topcoder uses {} for arrays, and LeetCode -- []
            throw std::invalid_argument("Expected a { or [ character, got " + std::string(1, c));
        std::vector<T> val;
        for (;;) {
            in >> c;
            if (c == '}' || c == ']')
                break;

            if (in.eof())
                throw std::invalid_argument("End of file reached while reading a vector");

            if (val.empty())
                in.putback(c);
            else if (c != ',')
                throw std::invalid_argument("Expected a comma, got " + std::string(1, c));

            T elem = Serializer<T>::deserialize(in);
            val.push_back(elem);
        }
        return val;
    }
};

}

