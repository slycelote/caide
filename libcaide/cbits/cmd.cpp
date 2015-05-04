#include <iostream>
#include <stdexcept>
#include <string>

#include "inliner.h"
#include "optimizer.h"

using namespace std;

int main(int argc, const char* argv[]) {
    try {
        vector<string> options, files;
        int i = 1;
        while (string(argv[i]) != "--") {
            options.push_back(argv[i]);
            ++i;
        }
        ++i;
        string cmd = argv[i];
        for (++i; i < argc; ++i)
            files.push_back(argv[i]);
        if (cmd == "inline") {
            Inliner inliner(options);
            for (const auto& f : files)
                cout << inliner.doInline(f);
        } else {
            vector<string> macrosToKeep;
            Optimizer optimizer(options, macrosToKeep);
            for (const auto& f : files)
                cout << optimizer.doOptimize(f);
        }
    } catch (const exception& e) {
        cerr << e.what() << endl;
        return 1;
    }

    return 0;
}

