#include "cwrapper.h"
#include "inliner.h"
#include "optimizer.h"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <stdexcept>

#include <cctype>

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

std::string trim(const std::string& s)
{
    auto isspace = [](char c) { return std::isspace(c) != 0; };
    auto wsfront = std::find_if_not(s.begin(), s.end(), isspace);
    auto wsback  = std::find_if_not(s.rbegin(), s.rend(), isspace).base();
    return wsback <= wsfront ? "" : std::string(wsfront, wsback);
}

FL_EXPORT_C(int, inline_code)(const char** cppFiles, int numCppFiles,
       const char** cmdLineOptions, int numCmdLineOptions,
       const char* outputFile)
{
    std::ofstream out(outputFile, std::ios::binary);
    try {
        const std::vector<std::string> cmdLineOptions_ = fromCharArrays(cmdLineOptions, numCmdLineOptions);
        Inliner inliner(cmdLineOptions_);
        for (int i = 0; i < numCppFiles; ++i) {
            std::string result = inliner.doInline(cppFiles[i]);
            result = trim(result);
            if (!result.empty())
                out << result << "\r\n";
        }
        return 0;
    } catch (const std::exception& e) {
        out << "Exception: " << e.what() << std::endl;
    } catch (...) {
        out << "Unexpected error" << std::endl;
    }
    return 42;
}

FL_EXPORT_C(int, remove_unused_code)(const char* cppFile,
       const char** cmdLineOptions, int numCmdLineOptions,
       const char** macrosToKeep, int numMacrosToKeep,
       const char* outputFile)
{
    std::ofstream out(outputFile, std::ios::binary);
    try {
        const std::vector<std::string> cmdLineOptions_ = fromCharArrays(cmdLineOptions, numCmdLineOptions);
        const std::vector<std::string> macrosToKeep_ = fromCharArrays(macrosToKeep, numMacrosToKeep);
        Optimizer optimizer(cmdLineOptions_, macrosToKeep_);
        std::string result = optimizer.doOptimize(cppFile);
        result = trim(result);
        out << result << "\r\n";
        return 0;
    } catch (const std::exception& e) {
        out << "Exception: " << e.what() << std::endl;
    } catch (...) {
        out << "Unexpected error" << std::endl;
    }
    return 43;
}

