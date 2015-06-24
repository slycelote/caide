#pragma once
#include <functional>
#include <cstdarg>
#include <cstdio>

class CIO {
private:
    std::function<int(const char*, va_list)> ioImpl;
public:
    explicit CIO(const std::function<int(const char*, va_list)>& impl);
    int operator()(const char* format, ...) const;
};

class FileHandle {
protected:
    FILE* handle;
    bool input;
    std::function<int(const char*, va_list)> ioFunc;
public:
    explicit FileHandle(const char* fileName, bool input);
    ~FileHandle();
};

class FileCIO : private FileHandle, public CIO {
public:
    FileCIO(const char* filePath, bool input);
};
