#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <typeinfo>
#include <vector>

using namespace std;

#ifdef CAIDE_TOPCODER
namespace caide_detail {

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
    static void serialize(ostream& out, T val) {
        out << val;
    }
    static T deserialize(istream& in) {
        T val;
        if (!(in >> val))
            throw std::invalid_argument(
                    string("Couldn't deserialize a value of type ") +
                    TypeName<T>::name());
        return val;
    }
};

template<>
struct Serializer<string> {
    static void serialize(ostream& out, const string& val) {
        out << '"' << val << '"';
    }
    static string deserialize(istream& in) {
        char c;
        in >> c;
        if (c != '"')
            throw std::invalid_argument("Expected a quote character");
        string s;
        for (;;) {
            in >> c;
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
struct Serializer<vector<T> > {
    static void serialize(ostream& out, const vector<T>& val) {
        out << '{';
        for (size_t i = 0; i < val.size(); ++i) {
            if (i > 0)
                out << ',';
            Serializer<T>::serialize(out, val[i]);
        }
        out << '}';
    }
    static vector<T> deserialize(istream& in) {
        char c;
        in >> c;
        if (c != '{')
            throw std::invalid_argument("Expected a { character");
        vector<T> val;
        for (;;) {
            in >> c;
            if (c == '}')
                break;

            if (in.eof())
                throw std::invalid_argument("End of file reached while reading a vector");

            if (val.empty())
                in.putback(c);
            else if (c != ',')
                throw std::invalid_argument("Expected a comma");

            T elem = Serializer<T>::deserialize(in);
            val.push_back(elem);
        }
        return val;
    }
};

template<typename T>
void tcread(istream& in, T& val) {
    val = Serializer<T>::deserialize(in);
}
template<typename T>
void tcwrite(ostream& out, const T& val) {
    Serializer<T>::serialize(out, val);
}

} // namespace caide_detail


// Code generator for Topcoder problem will define the following macros:
// - CAIDE_TC_RETURN_TYPE is the return value of the main method.
// - CAIDE_TC_PARAM_LIST calls CAIDE_TC_PARAM macro for each parameter of the main method.
//
// For example, the main method in the problem InfiniteString
//    (https://community.topcoder.com/stat?c=problem_statement&pm=13783)
//    has the following signature:
//
// string equal(string s, string t);
//
// Then, the following macros will be defined:
// #define CAIDE_TC_RETURN_TYPE string
// #define CAIDE_TC_PARAM_LIST  CAIDE_TC_PARAM(string, s) CAIDE_TC_PARAM(string, t)



// Forward declaration of solution class.
// Note: we cannot forward declare the actual class defined by Topcoder
// because we don't know what methods/members it will contain after it's implemented.
// So we use a special CaideSolution class.
// Its definition is in solution file and must not be modified! It will be removed before submission.
#define CAIDE_TC_PARAM(type, name) type name,
struct CaideSolution {
    CAIDE_TC_RETURN_TYPE solve(
        CAIDE_TC_PARAM_LIST
        int);
};
#undef CAIDE_TC_PARAM

void solve(istream& in, ostream& out) {
    out << std::setprecision(12);

    // Declare and read parameters
#define CAIDE_TC_PARAM(type, name) type name; caide_detail::tcread(in, name);
    CAIDE_TC_PARAM_LIST
#undef CAIDE_TC_PARAM

    // Run solution
    CaideSolution sol;
#define CAIDE_TC_PARAM(type, name) name,
    CAIDE_TC_RETURN_TYPE caide_result = sol.solve(
            CAIDE_TC_PARAM_LIST
            0);
#undef CAIDE_TC_PARAM

    // Write the result
    caide_detail::tcwrite(out, caide_result);
}


#else

// Must be defined in solution cpp file
void solve(istream& in, ostream& out);

#endif

namespace caide_detail {

static bool fileExists(const string& path) {
    ifstream f(path.c_str());
    return f.good();
}

#ifdef CAIDE_TOPCODER
void runTest(const char* inFile, const char* outFile, string& result) {
    ostringstream out;
    ifstream in(inFile);
    solve(in, out);
    result = out.str();
    ofstream resFile(outFile);
    resFile << result;
}
#else
// Note: this function is identical to the one above but you may want to
// change it, for example if you don't like using iostream library
void runTest(const char* inFile, const char* outFile, string& result) {
    ostringstream out;
    ifstream in(inFile);
    solve(in, out);
    result = out.str();
    ofstream resFile(outFile);
    resFile << result;
}
#endif

}

int main() {
    // Find test directory
    string testDir = "./";
    string caideExeFile = testDir + "caideExe.txt";
    if (!caide_detail::fileExists(caideExeFile)) {
        // support running from problem directory too
        testDir = ".caideproblem/test/";
        caideExeFile = testDir + "caideExe.txt";
    }
    if (!caide_detail::fileExists(caideExeFile)) {
        cerr << "Test must be run from either problem directory or .caideproblem/test subdirectory" << endl;
        return 9876;
    }

    // Read path to caide executable from a file in the test directory
    string caideExe;
    {
        ifstream caidePath(caideExeFile.c_str());
        getline(caidePath, caideExe);
#ifdef _WIN32
        string quotes = "\"\"";
#else
        string quotes = "\"";
#endif
        caideExe = quotes + caideExe + quotes;
    }

    // Prepare the list of test cases in correct order; add recently created test cases too.
    int ret = std::system((caideExe + " update_tests").c_str());
    if (ret != 0) {
        cerr << "caide update_tests returned non-zero error code " << ret << endl;
    }

    // Process each test case described in a file in current directory
    ifstream testList((testDir + "testList.txt").c_str());

    string testState, testName;
    while (testList >> testName >> testState) {
        if (testState == "Skip") {
            cerr << "Skipping test " << testName << endl;
            // mark test as skipped
            ofstream resFile((testDir + testName + ".skipped").c_str());
        } else if (testState == "Run") {
            cerr << "Running test " << testName << endl;
            string result;
            try {
                caide_detail::runTest((testDir + testName + ".in").c_str(),
                        (testDir + testName + ".out").c_str(),
                        result);
            } catch (...) {
                cerr << "Test " << testName << " threw an exception" << endl;
                // mark test as failed
                ofstream resFile((testDir + testName + ".failed").c_str());
                continue;
            }

            // optional: print program output to stderr
            if (result.size() > 200)
                result = result.substr(0, 200) + "[...] (output truncated)\n";
            cerr << result << endl;
        } else {
            // internal caide error: unknown test status
            ofstream resFile((testDir + testName + ".error").c_str());
        }
    }

    // optional: evaluate tests automatically
    ret = std::system((caideExe + " eval_tests").c_str());
    return ret;
}

