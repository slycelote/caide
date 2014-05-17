#include "cwrapper.h"
#include "inliner.h"
#include "optimizer.h"

#include <iostream>
#include <fstream>

std::vector<std::string> fromCharArrays(const char** a, int n) {
    std::vector<std::string> res;
    for (int i = 0; i < n; ++i)
        res.push_back(a[i]);
    return res;
}


FL_EXPORT_C(void, inline_code)(const char** cppFiles, int numCppFiles,
       const char** systemHeaders, int numSystemHeaders, const char* outputFile)
{
    std::cout << "Hello FFI!\n";
    for (int i = 0; i < numCppFiles; ++i)
        std::cout << i << ": " << cppFiles[i] << std::endl;
    for (int i = 0; i < numSystemHeaders; ++i)
        std::cout << i << ": " << systemHeaders[i] << std::endl;
    const std::vector<std::string> systemHeaders_ = fromCharArrays(systemHeaders, numSystemHeaders);
    Inliner inliner(systemHeaders_);
    std::ofstream out(outputFile);
    for (int i = 0; i < numCppFiles; ++i)
        out << inliner.doInline(cppFiles[i]) << "\n";
}

FL_EXPORT_C(void, remove_unused_code)(const char* cppFile,
       const char** systemHeaders, int numSystemHeaders, const char* outputFile)
{
    const std::vector<std::string> systemHeaders_ = fromCharArrays(systemHeaders, numSystemHeaders);
    Optimizer optimizer(systemHeaders_);
    std::ofstream out(outputFile);
    out << optimizer.doOptimize(cppFile) << "\n";
}

