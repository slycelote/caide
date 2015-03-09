#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include <cstdlib>

// Must be defined in solution cpp file
void solve(std::istream& in, std::ostream& out);


using namespace std;

static bool fileExists(const string& path) {
    ifstream f(path.c_str());
    return f.good();
}

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
            ostringstream out;
            ifstream in((testDir + testName + ".in").c_str());
            try {
                solve(in, out);
            } catch (...) {
                cerr << "Test " << testName << " threw an exception" << endl;
                // mark test as failed
                ofstream resFile((testDir + testName + ".failed").c_str());
                continue;
            }

            // save program output
            string result = out.str();
            {
                ofstream resFile((testDir + testName + ".out").c_str());
                resFile << result;
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

