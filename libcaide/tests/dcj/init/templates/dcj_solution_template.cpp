#include <iostream>
using namespace std;


extern "C" {
int NumberOfNodes();
int MyNodeId();
void PutChar(int target, char value);
void PutInt(int target, int value);
void PutLL(int target, long long value);
void Send(int target);
int Receive(int source);
char GetChar(int source);
int GetInt(int source);
long long GetLL(int source);
}




// Powered by caide (code generator, tester, and library code inliner)
class Solution {
public:
    void solve(ostream& out) {
    }
};

void solve(ostream& out)
{
//    out << std::setprecision(12);
    Solution solution;
    solution.solve(out);
}
