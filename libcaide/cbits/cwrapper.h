#ifndef CWRAPPER
#define CWRAPPER

#ifdef EXPORT
#undef EXPORT
#endif
#define EXPORT extern "C"

#ifdef __WATCOMC__
  #include <windows.h>
  #define FL_EXPORT_C(TYPE,FUNC_NAME) TYPE __export __cdecl FUNC_NAME
#else
  #ifdef _WIN32
    #define FL_EXPORT_C(TYPE,FUNC_NAME) TYPE __cdecl FUNC_NAME
    #undef EXPORT
    #define EXPORT extern "C"
  #else
    #define FL_EXPORT_C(TYPE,FUNC_NAME) TYPE FUNC_NAME
  #endif
  #ifndef _cdecl
    #define _cdecl
  #endif
#endif

#ifdef __cplusplus
EXPORT {
#endif
  FL_EXPORT_C(int, inline_code)(const char** cppFiles, int numCppFiles,
       const char** cmdLineOptions, int numCmdLineOptions,
       const char* outputFile);
  FL_EXPORT_C(int, remove_unused_code)(const char* cppFile,
       const char** cmdLineOptions, int numCmdLineOptions,
       const char** macrosToKeep, int numMacrosToKeep,
       const char* outputFile);
#ifdef __cplusplus
}
#endif
#endif /* CWRAPPER */

