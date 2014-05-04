#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

// Must be defined in solution cpp file
void solve(std::istream& in, std::ostream& out);


using namespace std;


int main() {
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

    return 0;
}

