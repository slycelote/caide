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


FL_EXPORT_C(void, inline_code)(const char** cppFiles, int numCppFiles,
       const char** systemHeaders, int numSystemHeaders,
       const char** userHeaders, int numUserHeaders, const char* outputFile)
{
    std::ofstream out(outputFile, std::ios::binary);
    try {
        const std::vector<std::string> systemHeaders_ = fromCharArrays(systemHeaders, numSystemHeaders);
        const std::vector<std::string> userHeaders_ = fromCharArrays(userHeaders, numUserHeaders);
        Inliner inliner(systemHeaders_, userHeaders_);
        for (int i = 0; i < numCppFiles; ++i)
            out << inliner.doInline(cppFiles[i]) << "\n";
    } catch (const std::exception& e) {
        out << "Exception: " << e.what() << std::endl;
    } catch (...) {
        out << "Unexpected error\n";
    }
}

FL_EXPORT_C(void, remove_unused_code)(const char* cppFile,
       const char** systemHeaders, int numSystemHeaders, const char* outputFile)
{
    std::ofstream out(outputFile, std::ios::binary);
    try {
        const std::vector<std::string> systemHeaders_ = fromCharArrays(systemHeaders, numSystemHeaders);
        Optimizer optimizer(systemHeaders_);
        out << optimizer.doOptimize(cppFile) << "\n";
    } catch (const std::exception& e) {
        out << "Exception: " << e.what() << std::endl;
    } catch (...) {
        out << "Unexpected error\n";
    }
}
