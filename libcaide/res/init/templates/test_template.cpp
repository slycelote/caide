#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include <cstdlib>

// Must be defined in solution cpp file
void solve(std::istream& in, std::ostream& out);


using namespace std;


int main() {
    // Read path to caide executable from a file in current directory
    string caideExe;
    {
        ifstream caidePath("caideExe.txt");
        caidePath >> caideExe;
    }

    // Prepare the list of test cases in correct order; add recently created test cases too.
    // Prepare submission file.
    int ret = std::system((caideExe + " update_tests").c_str());
    if (ret != 0) {
        cerr << "caide make returned non-zero error code " << ret << endl;
    }

    // Process each test case described in a file in current directory
    ifstream testList("testList.txt");

    string testState, testName;
    while (testList >> testName >> testState ) {
        if (testState == "Skip") {
            cerr << "Skipping test " << testName << endl;
            // mark test as skipped
            ofstream resFile((testName + ".skipped").c_str());
        } else if (testState == "Run") {
            cerr << "Running test " << testName << endl;
            ostringstream out;
            ifstream in((testName + ".in").c_str());
            try {
                solve(in, out);
            } catch (...) {
                cerr << "Test " << testName << "threw an exception" << endl;
                // mark test as failed
                ofstream resFile((testName + ".failed").c_str());
                continue;
            }

            // save program output
            string result = out.str();
            ofstream resFile((testName + ".out").c_str());
            resFile << result;

            // optional: print program output to stderr
            if (result.size() > 100)
                result = result.substr(0, 100) + "[...] (output truncated)\n";
            cerr << result << endl;
        } else {
            // internal caide error: unknown test status
            ofstream resFile((testName + ".error").c_str());
        }
    }

    // optional: evaluate tests automatically
    ret = std::system((caideExe + " eval_tests").c_str());
    return ret;
}

