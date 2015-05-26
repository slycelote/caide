#include "io.h"

CIO::CIO(const std::function<int(const char*, va_list)>& impl)
    : ioImpl(impl)
{}

int CIO::operator()(const char* format, ...) const {
    int res;
    va_list args;
    va_start(args, format);
    res = ioImpl(format, args);
    va_end(args);
    return res;
}

FileCIO::FileCIO(const char* filePath, bool input)
    : FileHandle(filePath, input)
    , CIO(ioFunc)
{
}

FileHandle::FileHandle(const char* filePath, bool input)
    : input(input)
{
    handle = fopen(filePath, input ? "r" : "w");
    if (input)
        ioFunc = [&](const char* format, va_list args) {
            return vfscanf(handle, format, args);
        }; 
    else
        ioFunc = [&](const char* format, va_list args) {
            return vfprintf(handle, format, args);
        }; 
}

FileHandle::~FileHandle() {
    fclose(handle);
}
