#define CAIDE_DCJ 1
#include <algorithm>
#include <condition_variable>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <mutex>
#include <queue>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>


#define CAIDE_DCJ_DEBUG
//#define CAIDE_DCJ_TRACE

#if defined(_MSC_VER) && _MSC_VER <= 1800
#define thread_local _declspec(thread)
#endif

using namespace std;

#include "dcj_tests.h"

namespace dcj {

int getNumTestCases() {
    return (int)sizeof(testCases) / sizeof(ITestCase*);
}

ITestCase& getTestCase() {
    if (testCaseNumber < 0) testCaseNumber = 0;
    if (testCaseNumber >= getNumTestCases())
       throw std::runtime_error("Unknown test case number");
    return *testCases[testCaseNumber];
}

void readDcjData(std::istream& in) {
    in >> testCaseNumber;
    if (testCaseNumber <= 0 || testCaseNumber >= getNumTestCases()) {
        throw std::runtime_error("CustomTestCase not implemented");
    }
}

}




namespace caide { namespace dcj {

struct MessagePart {
#ifdef CAIDE_DCJ_DEBUG
    enum class Type { CHAR, INT, LL };

    Type type;
#endif

    union {
        char ch;
        int i;
        long long ll;
    } value;
};

using Message = std::queue<MessagePart>;

class Node {
private:
    std::vector<Message> sendBuffers;
    std::vector<Message> receiveBuffers;
    // TODO: replace with a single list + N vectors of iterators into the list
    std::vector<std::queue<std::pair<int, Message>>> queue;
    std::mutex queueLock;
    std::condition_variable queueNotify;
    const int id;

#ifdef CAIDE_DCJ_DEBUG
    void verifyNext(int source, MessagePart::Type expected) const {
        static std::vector<std::string> names { "CHAR", "INT", "LL" };
        auto actual = receiveBuffers[source].front().type;
        if (actual != expected)
            throw std::logic_error("Unexpected message (wrong Get* function called). Node ID " + to_string(id) + ". " +
                                   "Expected " + names[(int)expected] + ", actual " + names[(int)actual]);
    }
#endif

    Node(int nodeId, int nodeCount)
        : sendBuffers(nodeCount)
        , receiveBuffers(nodeCount)
        , queue(nodeCount)
        , id(nodeId)
    {}

public:
    static thread_local int myId;
    static std::vector<std::unique_ptr<Node>> nodes;

    Node(Node&&) = delete;
    Node(Node&)  = delete;

    static void initialize(int nodeCount) {
        nodes.clear();
        for (int i = 0; i < nodeCount; ++i)
            nodes.emplace_back(new Node(i, nodeCount));
    }

    // Main function of the worker thread
    void run(std::function<void()> solution) {
        myId = id;
        solution();
#ifdef CAIDE_DCJ_TRACE
        std::cerr << "Finished node ID " << id << std::endl;
#endif
    }

    // Append "value" to the message that is being prepared for the node with id
    // "target".
    void putChar(int target, char value) {
        MessagePart m;
#ifdef CAIDE_DCJ_DEBUG
        m.type = MessagePart::Type::CHAR;
        if (target < 0 || target >= (int)queue.size())
            throw logic_error(string("PutChar(" + to_string(target) + ") called"));
#endif
        m.value.ch = value;
        sendBuffers[target].emplace(m);
    }

    void putInt(int target, int value) {
        MessagePart m;
#ifdef CAIDE_DCJ_DEBUG
        m.type = MessagePart::Type::INT;
        if (target < 0 || target >= (int)queue.size())
            throw logic_error(string("PutInt(" + to_string(target) + ") called"));
#endif
        m.value.i = value;
        sendBuffers[target].emplace(m);
    }

    void putLL(int target, long long value) {
        MessagePart m;
#ifdef CAIDE_DCJ_DEBUG
        m.type = MessagePart::Type::LL;
        if (target < 0 || target >= (int)queue.size())
            throw logic_error(string("PutLL(" + to_string(target) + ") called"));
#endif
        m.value.ll = value;
        sendBuffers[target].emplace(m);
    }

    // Sends the message that was accumulated in the appropriate buffer to the
    // "target" instance, and clear the buffer for this instance.
    //
    // This method is non-blocking - that is, it does not wait for the receiver to
    // call "Receive", it returns immediately after sending the message.
    void send(int target) {
#ifdef CAIDE_DCJ_TRACE
        std::cerr << "Sending " << id << " -> " << target << "[o]" << std::endl;
#endif
#ifdef CAIDE_DCJ_DEBUG
        if (target < 0 || target >= (int)queue.size())
            throw logic_error(string("Send(" + to_string(target) + ") called"));
#endif
        std::unique_lock<std::mutex> lock(nodes[target]->queueLock);
        nodes[target]->queue[id].emplace(id, std::move(sendBuffers[target]));
        sendBuffers[target] = Message{};
        lock.unlock();
#ifdef CAIDE_DCJ_TRACE
        std::cerr << "Sent!   " << id << " -> " << target << "[x]" << std::endl;
#endif
        nodes[target]->queueNotify.notify_one();
    }

    // The library also has a receiving buffer for each instance. When you call
    // "Receive" and retrieve a message from an instance, the buffer tied to this
    // instance is overwritten. You can then retrieve individual parts of the
    // message through the Get* methods. You must retrieve the contents of the
    // message in the order in which they were appended.
    //
    // This method is blocking - if there is no message to receive, it will wait for
    // the message to arrive.
    //
    // You can call Receive(-1) to retrieve a message from any source, or with with
    // source in [0 .. NumberOfNodes()-1] to retrieve a message from a particular
    // source.
    //
    // It returns the number of the instance which sent the message (which is equal
    // to source, unless source is -1).
    int receive(int source) {
#ifdef CAIDE_DCJ_TRACE
        std::cerr << "Receiving " << id << " <- " << source << "[o]" << std::endl;
#endif
#ifdef CAIDE_DCJ_DEBUG
        if (source < -1 || source >= (int)queue.size())
            throw logic_error(string("Receive(" + to_string(source) + ") called"));
#endif
        std::unique_lock<std::mutex> lock(queueLock);
        queueNotify.wait(lock, [this, &source] {
            if (source >= 0)
                return !queue[source].empty();
            auto it = std::find_if(queue.begin(), queue.end(), [](const std::queue<std::pair<int, Message>>& q) { return !q.empty(); });
            if (it == queue.end())
                return false;
            source = std::distance(queue.begin(), it);
            return true;
        });

        receiveBuffers[source] = std::move(queue[source].front().second);
        queue[source].pop();
#ifdef CAIDE_DCJ_TRACE
        std::cerr << "Received! " << id << " <- " << source << "[x]" << std::endl;
#endif
        return source;
    }

    // Each of these methods returns and consumes one item from the buffer of the
    // appropriate instance. You must call these methods in the order in which the
    // elements were appended to the message (so, for instance, if the message was
    // created with PutChar, PutChar, PutLL, you must call GetChar, GetChar, GetLL
    // in this order).
    // If you call them in different order, or you call a Get* method after
    // consuming all the contents of the buffer, behaviour is undefined.
    char getChar(int source) {
#ifdef CAIDE_DCJ_DEBUG
        verifyNext(source, MessagePart::Type::CHAR);
        if (source < -1 || source >= (int)queue.size())
            throw logic_error(string("GetChar(" + to_string(source) + ") called"));
#endif
        char value = receiveBuffers[source].front().value.ch;
        receiveBuffers[source].pop();
        return value;
    }

    int getInt(int source) {
#ifdef CAIDE_DCJ_DEBUG
        verifyNext(source, MessagePart::Type::INT);
        if (source < -1 || source >= (int)queue.size())
            throw logic_error(string("GetInt(" + to_string(source) + ") called"));
#endif
        int value = receiveBuffers[source].front().value.i;
        receiveBuffers[source].pop();
        return value;
    }

    long long getLL(int source) {
#ifdef CAIDE_DCJ_DEBUG
        verifyNext(source, MessagePart::Type::LL);
        if (source < -1 || source >= (int)queue.size())
            throw logic_error(string("GetLL(" + to_string(source) + ") called"));
#endif
        long long value = receiveBuffers[source].front().value.ll;
        receiveBuffers[source].pop();
        return value;
    }
};

thread_local int Node::myId = 0;
std::vector<std::unique_ptr<Node>> Node::nodes;

} }


extern "C" {

// The number of nodes on which the solution is running.
int NumberOfNodes() { return (int)caide::dcj::Node::nodes.size(); }

// The number (in the range [0 .. NumberOfNodes()-1]) of the node on which this
// process is running.
int MyNodeId() {
    return caide::dcj::Node::myId;
}

// In all the methods below, if "target" or "source" is not in the valid range,
// the behaviour is undefined.

// The library internally has a message buffer for each of the nodes in
// [0 .. NumberOfNodes()-1]. It accumulates the message in such a buffer through
// the "Put" methods.

// Append "value" to the message that is being prepared for the node with id
// "target".
void PutChar(int target, char value) { caide::dcj::Node::nodes[MyNodeId()]->putChar(target, value); }
void PutInt(int target, int value) { caide::dcj::Node::nodes[MyNodeId()]->putInt(target, value); }
void PutLL(int target, long long value) { caide::dcj::Node::nodes[MyNodeId()]->putLL(target, value); }

// Sends the message that was accumulated in the appropriate buffer to the
// "target" instance, and clear the buffer for this instance.
//
// This method is non-blocking - that is, it does not wait for the receiver to
// call "Receive", it returns immediately after sending the message.
void Send(int target) { caide::dcj::Node::nodes[MyNodeId()]->send(target); }

// The library also has a receiving buffer for each instance. When you call
// "Receive" and retrieve a message from an instance, the buffer tied to this
// instance is overwritten. You can then retrieve individual parts of the
// message through the Get* methods. You must retrieve the contents of the
// message in the order in which they were appended.
//
// This method is blocking - if there is no message to receive, it will wait for
// the message to arrive.
//
// You can call Receive(-1) to retrieve a message from any source, or with with
// source in [0 .. NumberOfNodes()-1] to retrieve a message from a particular
// source.
//
// It returns the number of the instance which sent the message (which is equal
// to source, unless source is -1).
int Receive(int source) { return caide::dcj::Node::nodes[MyNodeId()]->receive(source); }

// Each of these methods returns and consumes one item from the buffer of the
// appropriate instance. You must call these methods in the order in which the
// elements were appended to the message (so, for instance, if the message was
// created with PutChar, PutChar, PutLL, you must call GetChar, GetChar, GetLL
// in this order).
// If you call them in different order, or you call a Get* method after
// consuming all the contents of the buffer, behaviour is undefined.
char GetChar(int source) { return caide::dcj::Node::nodes[MyNodeId()]->getChar(source); }
int GetInt(int source) { return caide::dcj::Node::nodes[MyNodeId()]->getInt(source); }
long long GetLL(int source) { return caide::dcj::Node::nodes[MyNodeId()]->getLL(source); }

} // extern "C"


/** Custom checker **/
static const bool USE_CUSTOM_CHECKER = false;

bool customChecker(istream& input, istream& userOutput, istream& judgeOutput,
                   string& errorMessage)
{
    (void)input; (void)userOutput; (void)judgeOutput;

    errorMessage = "";

    return true;
}

/** Test generator **/
/*
static void generator() {
}
*/

static void runTest(const char* inFile, const char* outFile, string& result);

// The function that must be defined in solution CPP file
void solve(ostream& out);

// Test method calling the solution function. Must write result to the output
// file and into the string.
static void runTest(const char* inFile, const char* outFile, string& result) {
    ostringstream out;
    ifstream in(inFile);
    dcj::readDcjData(in);

    using namespace caide::dcj;

    // TODO: configurable node count
    const int nodeCount = 4;

    Node::initialize(nodeCount);

    std::vector<std::thread> workerThreads;
    for (int i = 0; i < nodeCount; ++i) {
        workerThreads.emplace_back([&, i] {
            Node::nodes[i]->run([&] { solve(out); });
        });
    }
    for (std::thread& t : workerThreads)
        t.join();

    result = out.str();
    ofstream resFile(outFile);
    resFile << result;
}


/** ======================================================================= **/

static bool fileExists(const string& path);

static int getSubprocessExitCode(int systemRet);

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
            } catch (const std::exception& e) {
                cerr << "Test " << testName << " threw an exception" << endl;
                cerr << e.what() << endl;
                report << testName << " failed" << endl;
                continue;
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
    return getSubprocessExitCode(ret);
}


static bool fileExists(const string& path) {
    ifstream f(path.c_str());
    return f.good();
}

static int getSubprocessExitCode(int systemRet) {
    // Correct implementation is non-portable (e.g. on Linux use WEXITSTATUS)
    if (systemRet % 256 == 0)
        systemRet /= 256;
    return systemRet;
}


