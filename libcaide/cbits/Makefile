# The following variables will likely need to be customized, depending on where
# and how you built LLVM & Clang. They can be overridden by setting them on the
# make command line: "make VARNAME=VALUE", etc.

# LLVM_SRC_PATH is the path to the root of the checked out source code. This
# directory should contain the configure script, the include/ and lib/
# directories of LLVM, Clang in tools/clang/, etc.
# Alternatively, if you're building vs. a binary download of LLVM, then
# LLVM_SRC_PATH can point to the main untarred directory.
ifdef CAIDE_DEBUG
	LLVM_SRC_PATH := clangbuilddebug/out
else
	LLVM_SRC_PATH := clangbuild/out
endif

# LLVM_BUILD_PATH is the directory in which you built LLVM - where you ran
# configure or cmake.
# For linking vs. a binary build of LLVM, point to the main untarred directory.
# LLVM_BIN_PATH is the directory where binaries are placed by the LLVM build
# process. It should contain the tools like opt, llc and clang.
LLVM_BUILD_PATH := $(LLVM_SRC_PATH)
LLVM_BIN_PATH := $(LLVM_BUILD_PATH)/bin

# It's recommended that CXX matches the compiler you used to build LLVM itself.
CXX := g++
CXXFLAGS := -c -fno-rtti -static-libstdc++ -static-libgcc -std=c++11 -Wall -Wextra -Wshadow -Wlogical-op -D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS
ifdef CAIDE_DEBUG
	CXXFLAGS += -g
else
	CXXFLAGS += -O2
endif

ARCHIVER := ar

LLVM_CXXFLAGS := $($(LLVM_BIN_PATH)/llvm-config --cxxflags)

CLANG_INCLUDES := \
	-isystem $(LLVM_SRC_PATH)/include \
	-isystem $(LLVM_BUILD_PATH)/include

BUILDDIR := build

.PHONY: all
all: make_builddir $(BUILDDIR)/libcpphelper.a $(BUILDDIR)/libchelper.a


.PHONY: make_builddir
make_builddir:
	@test -d $(BUILDDIR) || mkdir $(BUILDDIR)

$(BUILDDIR)/libcpphelper.a: $(BUILDDIR)/inliner.o $(BUILDDIR)/optimizer.o $(BUILDDIR)/SmartRewriter.o $(BUILDDIR)/util.o $(BUILDDIR)/RemoveInactivePreprocessorBlocks.o $(BUILDDIR)/StmtParentMap.o
	$(ARCHIVER) rcs $(BUILDDIR)/libcpphelper.a $^

$(BUILDDIR)/cwrapper.o: cwrapper.cpp eol.h
	$(CXX) $(CXXFLAGS) cwrapper.cpp -o $(BUILDDIR)/cwrapper.o -Lbuild -lcpphelper

$(BUILDDIR)/libchelper.a: $(BUILDDIR)/libcpphelper.a $(BUILDDIR)/cwrapper.o
	$(ARCHIVER) rcs $(BUILDDIR)/libchelper.a $(BUILDDIR)/cwrapper.o

$(BUILDDIR)/inliner.o: inliner.cpp util.h inliner.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) inliner.cpp -o $@

$(BUILDDIR)/optimizer.o: optimizer.cpp util.h optimizer.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) optimizer.cpp -o $@

$(BUILDDIR)/SmartRewriter.o: SmartRewriter.cpp SmartRewriter.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) SmartRewriter.cpp -o $@

$(BUILDDIR)/RemoveInactivePreprocessorBlocks.o: RemoveInactivePreprocessorBlocks.cpp RemoveInactivePreprocessorBlocks.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) RemoveInactivePreprocessorBlocks.cpp -o $@

$(BUILDDIR)/StmtParentMap.o: StmtParentMap.cpp StmtParentMap.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) StmtParentMap.cpp -o $@

$(BUILDDIR)/util.o: util.cpp util.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $(CLANG_INCLUDES) util.cpp -o $@

.PHONY: clean
clean:
	rm -rf $(BUILDDIR)/* *.dot

######## Test targets
console_test: $(BUILDDIR)/cmd.exe

LLVM_LDFLAGS := $($(LLVM_BIN_PATH)/llvm-config --libs)

CLANG_LIBS := \
	-lchelper \
	-lcpphelper \
	-lclangTooling \
	-lclangFrontendTool \
	-lclangFrontend \
	-lclangDriver \
	-lclangSerialization \
	-lclangCodeGen \
	-lclangParse \
	-lclangSema \
	-lclangAnalysis \
	-lclangRewriteFrontend \
	-lclangRewrite \
	-lclangEdit \
	-lclangAST \
	-lclangLex \
	-lclangBasic \
	-lLLVMInstrumentation \
	-lLLVMIRReader \
	-lLLVMAsmParser \
	-lLLVMDebugInfo \
	-lLLVMOption \
	-lLLVMLTO \
	-lLLVMLinker \
	-lLLVMipo \
	-lLLVMVectorize \
	-lLLVMBitWriter \
	-lLLVMBitReader \
	-lLLVMTableGen \
	-lLLVMX86Disassembler \
	-lLLVMX86AsmParser \
	-lLLVMX86CodeGen \
	-lLLVMSelectionDAG \
	-lLLVMAsmPrinter \
	-lLLVMX86Desc \
	-lLLVMX86Info \
	-lLLVMX86AsmPrinter \
	-lLLVMX86Utils \
	-lLLVMMCDisassembler \
	-lLLVMMCParser \
	-lLLVMInterpreter \
	-lLLVMMCJIT \
	-lLLVMCodeGen \
	-lLLVMObjCARCOpts \
	-lLLVMScalarOpts \
	-lLLVMInstCombine \
	-lLLVMTransformUtils \
	-lLLVMipa \
	-lLLVMAnalysis \
	-lLLVMRuntimeDyld \
	-lLLVMExecutionEngine \
	-lLLVMTarget \
	-lLLVMMC \
	-lLLVMObject \
	-lLLVMCore \
	-lLLVMSupport

$(BUILDDIR)/cmd.exe: $(BUILDDIR)/libcpphelper.a $(BUILDDIR)/libchelper.a $(BUILDDIR)/cmd.o
	$(CXX) -static-libstdc++ -static-libgcc -Lbuild -L$(LLVM_BUILD_PATH)/lib $(BUILDDIR)/cmd.o $(CLANG_LIBS) -ldl -o $(BUILDDIR)/cmd.exe

$(BUILDDIR)/cmd.o: cmd.cpp
	$(CXX) $(CXXFLAGS) cmd.cpp -o $(BUILDDIR)/cmd.o

