#include <vector>
#include <set>
#include <string>

class Optimizer {
public:
    Optimizer(const std::vector<std::string>& cmdLineOptions,
              const std::vector<std::string>& macrosToKeep);
    std::string doOptimize(const std::string& cppFile);

private:
    std::vector<std::string> cmdLineOptions;
    std::set<std::string> macrosToKeep;
};

