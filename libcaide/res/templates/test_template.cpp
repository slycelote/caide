#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

using namespace std;

#ifdef CAIDE_TOPCODER
#include "topcoder_serialize.h"
template<typename T>
void tcread(istream& in, T& val) {
    val = caide_tc::Serializer<T>::deserialize(in);
}
template<typename T>
void tcwrite(ostream& out, const T& val) {
    caide_tc::Serializer<T>::serialize(out, val);
}
#endif



/** Custom checker **/
static const bool USE_CUSTOM_CHECKER = false;

bool customChecker(istream& input, istream& userOutput, istream& judgeOutput,
                   string& errorMessage)
{
#ifdef CAIDE_TOPCODER
    // Declare and read return value
    CAIDE_TC_RETURN_TYPE result; tcread(userOutput, result);
    // Declare and read judge return value
    CAIDE_TC_RETURN_TYPE judgeResult; tcread(judgeOutput, judgeResult);
    // Declare and read input parameters
    char caide_char;
#define CAIDE_TC_PARAM(type, name) type name; input >> caide_char; tcread(input, name);
    CAIDE_TC_PARAM_LIST
#undef CAIDE_TC_PARAM
#endif

    errorMessage = "";

    return true;
}

/** Test generator **/
/*
static void generator() {
}
*/

static void runTest(const char* inFile, const char* outFile, string& result);

#ifndef CAIDE_TOPCODER
// The function that must be defined in solution CPP file
void solve(istream& in, ostream& out);

// Test method calling the solution function. Must write result to the output
// file and into the string.
static void runTest(const char* inFile, const char* outFile, string& result) {
    ostringstream out;
    ifstream in(inFile);
    solve(in, out);
    result = out.str();
    ofstream resFile(outFile);
    resFile << result;
}
#endif


/** ======================================================================= **/

static bool fileExists(const string& path);

int main() {
    // Find test directory
    string testDir = "./";
    string caideExeFile = testDir + "caideExe.txt";
    if (!fileExists(caideExeFile)) {
        // support running from problem directory too
        testDir = ".caideproblem/test/";
        caideExeFile = testDir + "caideExe.txt";
    }
    if (!fileExists(caideExeFile)) {
        cerr << "Test must be run from problem directory" << endl;
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

    ostringstream report;
    string testState, testName;
    while (testList >> testName >> testState) {
        if (testState == "Skip") {
            cerr << "Skipping test " << testName << endl;
            report << testName << " skipped" << endl;
        } else if (testState == "Run") {
            cerr << "Running test " << testName << endl;
            string result;
            try {
                runTest((testDir + testName + ".in").c_str(),
                        (testDir + testName + ".out").c_str(),
                        result);
            } catch (...) {
                cerr << "Test " << testName << " threw an exception" << endl;
                report << testName << " failed" << endl;
                continue;
            }

            if (USE_CUSTOM_CHECKER) {
                try {
                    istringstream output(result);
                    ifstream input((testDir + testName + ".in").c_str());
                    ifstream judgeOutput((testDir + "../../" + testName + ".out").c_str());
                    string message;
                    bool ok = customChecker(input, output, judgeOutput, message);
                    if (ok) {
                        report << testName << " OK" << endl;
                    } else {
                        cerr << "FAILED: " << message << endl;
                        report << testName << " failed " << message << endl;
                    }
                } catch (...) {
                    cerr << "Checker for test " << testName << " threw an exception" << endl;
                    report << testName << " error" << endl;
                    continue;
                }
            } else {
                report << testName << " ran" << endl;
            }

            // print program output to stderr
            if (result.size() > 200)
                result = result.substr(0, 200) + "[...] (output truncated)\n";
            cerr << result << endl;
        } else {
            report << testName << " error unknown test status" << endl;
        }
    }

    {
        ofstream reportFile((testDir + "report.txt").c_str());
        reportFile << report.str();
    }

    ret = std::system((caideExe + " eval_tests").c_str());
    return ret;
}


static bool fileExists(const string& path) {
    ifstream f(path.c_str());
    return f.good();
}

#ifdef CAIDE_TOPCODER

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


// Note: we cannot forward declare the actual class defined by Topcoder
// because we don't know what methods/members it will contain after it's implemented.
// So we declare a wrapper function around the solution class.
// Its definition is in solution file and must not be modified! It will be removed before submission.
#define CAIDE_TC_PARAM(type, name) type name,
CAIDE_TC_RETURN_TYPE solve(
    CAIDE_TC_PARAM_LIST
    int);
#undef CAIDE_TC_PARAM

void solve(istream& in, ostream& out) {
    out << std::setprecision(12);

    // Declare and read parameters
    char caide_char;
#define CAIDE_TC_PARAM(type, name) type name; in >> caide_char; tcread(in, name);
    CAIDE_TC_PARAM_LIST
#undef CAIDE_TC_PARAM

    // Run solution
#define CAIDE_TC_PARAM(type, name) name,
    CAIDE_TC_RETURN_TYPE caide_result = solve(
            CAIDE_TC_PARAM_LIST
            0);
#undef CAIDE_TC_PARAM

    // Write the result
    tcwrite(out, caide_result);
}


static void runTest(const char* inFile, const char* outFile, string& result) {
    ostringstream out;
    ifstream in(inFile);
    solve(in, out);
    result = out.str();
    ofstream resFile(outFile);
    resFile << result;
}
#endif
