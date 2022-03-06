#include "custom_checker.h"
#include "test_util.h"
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

using namespace std;

void solve(istream& in, ostream& out);

static const bool USE_CUSTOM_CHECKER = false;


/** ======================================================================= **/

static bool fileExists(const string& path) {
    ifstream f(path.c_str());
    return f.good();
}

static int runCommand(bool throwOnFailure, const vector<string>& cmdLine) {
    ostringstream os;

#ifdef _WIN32
    // std::system runs command as 'cmd /C'; see cmd.exe /? for details on quotes.
    os << "\"";
#endif
    for (size_t i = 0; i < cmdLine.size(); ++i) {
        if (i > 0)
            os << ' ';
        os << "\"" << cmdLine[i] << "\"";
    }

#ifdef _WIN32
    os << "\"";
#endif

    int res = system(os.str().c_str());
    if (throwOnFailure) {
        ostringstream err;
        err << "Command returned non-zero exit code " << res << ": " << err.str();
        throw runtime_error(err.str());
    }

    return res;
}

static int runCommand(bool throwOnFailure, const string& exe, const string& arg1,
    const string& arg2 = "", const string& arg3 = "")
{
    vector<string> cmdLine;
    cmdLine.push_back(exe);
    cmdLine.push_back(arg1);
    if (!arg2.empty()) cmdLine.push_back(arg2);
    if (!arg3.empty()) cmdLine.push_back(arg3);
    return runCommand(throwOnFailure, cmdLine);
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
        cerr << "Test must be run from problem directory" << endl;
        return 9876;
    }

    // Read path to caide executable from a file in the test directory
    string caideExe;
    {
        ifstream caidePath(caideExeFile.c_str());
        getline(caidePath, caideExe);
    }

    // Prepare the list of test cases in correct order; add recently created test cases too.
    int ret = runCommand(false, caideExe, "update_tests");
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
            string origInputFile = testDir + "../../" + testName + ".in";
#if defined(CAIDE_TOPCODER) || defined(CAIDE_LEETCODE)
            string inputFile = testDir + testName + ".plain.in";
            ret = runCommand(false, caideExe, "convert_test_input", origInputFile, inputFile);
            if (ret != 0) {
                cerr << "Test converter for test " << testName << " failed" << endl;
                report << testName << " error Test input converter failed" << endl;
                continue;
            }
#else
            string inputFile = origInputFile;
#endif
            string origOutputFile = testDir + testName + ".out";
            caide_tester::Stopwatch stopwatch;
            try {
                ifstream in(inputFile.c_str());
                ofstream out(origOutputFile.c_str());
                solve(in, out);
            } catch (const exception& e) {
                cerr << "Test " << testName << " threw an exception: " << e.what() << endl;
                report << testName << stopwatch.GetDuration() << " failed " << e.what() << endl;
                continue;
            } catch (...) {
                cerr << "Test " << testName << " threw an exception" << endl;
                report << testName << stopwatch.GetDuration() << " failed" << endl;
                continue;
            }

            string duration = stopwatch.GetDuration();

            if (USE_CUSTOM_CHECKER) {
                try {
                    string origEtalonFile = testDir + "../../" + testName + ".out";
#if defined(CAIDE_TOPCODER) || defined(CAIDE_LEETCODE)
                    string etalonFile = testDir + testName + ".plain.etalon";
                    runCommand(true, caideExe, "convert_test_output", origEtalonFile, etalonFile);
                    string outputFile = testDir + testName + ".plain.out";
                    runCommand(true, caideExe, "convert_test_output", origOutputFile, outputFile);
#else
                    string etalonFile = testDir + "../../" + testName + ".out";
                    string outputFile = origOutputFile;
#endif
                    ifstream output(outputFile.c_str());
                    ifstream input(inputFile.c_str());
                    ifstream judgeOutput(etalonFile.c_str());
                    string message;
                    bool ok = caide_tester::customChecker(input, output, judgeOutput, message);
                    if (ok) {
                        report << testName << duration << " OK" << endl;
                    } else {
                        cerr << "FAILED: " << message << endl;
                        report << testName << duration << " failed " << message << endl;
                    }
                } catch (const exception& e) {
                    cerr << "Checker for test " << testName << " threw an exception: " << e.what() << endl;
                    report << testName << duration << " error Custom checker failed: " << e.what() << endl;
                    continue;
                } catch (...) {
                    cerr << "Checker for test " << testName << " threw an exception" << endl;
                    report << testName << duration << " error Custom checker failed" << endl;
                    continue;
                }
            } else {
                report << testName << duration << " ran" << endl;
            }

            {
                // print program output to stderr
                vector<char> buf(200);
                ifstream is(origOutputFile.c_str());
                is.read(buf.data(), buf.size());
                cerr << string(buf.begin(), buf.end());
                if ((size_t)is.gcount() == buf.size())
                    cerr << "[...] (output truncated)\n";
                cerr << endl;
            }
        } else {
            report << testName << " error unknown test status" << endl;
        }
    }

    {
        ofstream reportFile((testDir + "report.txt").c_str());
        reportFile << report.str();
    }

    ret = runCommand(false, caideExe, "eval_tests");
    return ret;
}
