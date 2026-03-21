#include "mycc/Sema/Analyses/LabelGenerator.hpp"

using namespace mycc;

std::string LabelGenerator::generateLoopLabel() {
    return "loop_" + std::to_string(LoopLabelCounter++);
}

std::string LabelGenerator::generateSwitchLabel() {
    return "switch_" + std::to_string(SwitchLabelCounter++);
}

std::string LabelGenerator::generateCaseLabel() {
    return "case_" + std::to_string(CaseLabelCounter++);
}

std::string LabelGenerator::generateDefaultLabel() {
    return "default_" + std::to_string(DefaultLabelCounter++);
}