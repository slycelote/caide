#include "cwrapper.h"
#include "inliner.h"
#include "optimizer.h"

#include <iostream>
#include <fstream>
#include <stdexcept>

std::vector<std::string> fromCharArrays(const char** a, int n) {
    if (!a)
        throw std::runtime_error("Null pointer");
    std::vector<std::string> res;
    for (int i = 0; i < n; ++i) {
        if (!(a[i]))
            throw std::runtime_error("Null pointer");
        res.push_back(a[i]);
    }
    return res;
}


FL_EXPORT_C(int, inline_code)(const char** cppFiles, int numCppFiles,
       const char** cmdLineOptions, int numCmdLineOptions,
       const char* outputFile)
{
    std::ofstream out(outputFile, std::ios::binary);
    try {
        const std::vector<std::string> cmdLineOptions_ = fromCharArrays(cmdLineOptions, numCmdLineOptions);
        Inliner inliner(cmdLineOptions_);
        for (int i = 0; i < numCppFiles; ++i)
            out << inliner.doInline(cppFiles[i]) << "\n";
        return 0;
    } catch (const std::exception& e) {
        out << "Exception: " << e.what() << std::endl;
    } catch (...) {
        out << "Unexpected error\n";
    }
    return 42;
}

FL_EXPORT_C(int, remove_unused_code)(const char* cppFile,
       const char** cmdLineOptions, int numCmdLineOptions,
       const char* outputFile)
{
    std::ofstream out(outputFile, std::ios::binary);
    try {
        const std::vector<std::string> cmdLineOptions_ = fromCharArrays(cmdLineOptions, numCmdLineOptions);
        Optimizer optimizer(cmdLineOptions_);
        out << optimizer.doOptimize(cppFile) << "\n";
        return 0;
    } catch (const std::exception& e) {
        out << "Exception: " << e.what() << std::endl;
    } catch (...) {
        out << "Unexpected error\n";
    }
    return 43;
}

