#include <vector>
#include <string>

class Optimizer {
public:
    explicit Optimizer(const std::vector<std::string>& systemHeadersDirectories);
    std::string doOptimize(const std::string& cppFile);

private:
    std::vector<std::string> systemHeadersDirectories;
};

