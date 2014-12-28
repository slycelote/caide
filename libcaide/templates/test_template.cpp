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
    ifstream("caideExe.txt") >> caideExe;

    // Prepare the list of test cases in correct order; add recently created test cases too.
    // Prepare submission file.
    int ret = std::system((caideExe + " make").c_str());
    if (ret != 0) {
        cerr << "caide make returned non-zero error code " << ret << endl;
    }

    // Process each test case described in a file in current directory
    ifstream testList("testList.txt");

    string testState, testName;
    while (testList >> testName >> testState ) {
        if (testState == "Skip") {
            cerr << "Skipping test " << testName << endl;
            ofstream resFile((testName + ".skipped").c_str());
        } else if (testState == "Run") {
            cerr << "Running test " << testName << endl;
            ostringstream out;
            ifstream in((testName + ".in").c_str());
            try {
                solve(in, out);
            } catch (...) {
                cerr << "Test " << testName << "threw an exception" << endl;
                ofstream resFile((testName + ".failed").c_str());
                continue;
            }
            string result = out.str();
            ofstream resFile((testName + ".out").c_str());
            resFile << result;
        } else {
            ofstream resFile((testName + ".error").c_str());
        }
    }

    ret = std::system((caideExe + " test").c_str());
    if (ret != 0) {
        cerr << "caide test returned non-zero error code " << ret << endl;
    }

    return ret;
}

