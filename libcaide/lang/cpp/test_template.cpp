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
    while (testList >> testState >> testName) {

        if (testState == "skip") {
            cerr << "Skipping test " << testName << endl;
            ofstream resFile(testName + ".skipped");
        } else if (testState == "run") {
            ostringstream out;
            ifstream in(testName + ".in");
            try {
                solve(in, out);
            } catch (...) {
                cerr << "Test " << testName << "threw an exception" << endl;
                ofstream resFile(testName + ".failed");
                continue;
            }
            string result = out.str();
            ofstream resFile(testName + ".out");
            resFile << result;
        } else {
            ofstream resFile(testName + ".error");
        }
    }

    return 0;
}

