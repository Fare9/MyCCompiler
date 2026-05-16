#include "mycc/AST/Type.hpp"

using namespace mycc;

int BuiltinType::integerRank(BuiltinType::BuiltinKind K) {
    switch (K) {
        case BuiltinType::Bool: return 0;
        case BuiltinType::Char: return 1;
        case BuiltinType::Short: return 2;
        case BuiltinType::Int: return 3;
        case BuiltinType::Long: return 4;
        default: return -1; // Void or unknown — not promotable
    }
}

std::string BuiltinType::to_string() {
    switch (BuiltinK) {
        case Bool:
            return "bool";
        case Char:
            return "char";
        case Short:
            return "short";
        case Int:
            return "int";
        case Long:
            return "long";
        case Void:
            return "void";
        default:
            return "";
    }
}

bool BuiltinType::equal(const Type &other) const {
    if (other.getKind() != TK_Builtin) return false;
    return BuiltinK == llvm::cast<BuiltinType>(&other)->getBuiltinKind();
}

std::string FunctionType::to_string() {
    if (!funcTypeStr.empty())
        return funcTypeStr;
    funcTypeStr += retType->to_string() + " ";
    funcTypeStr += "(";
    for (auto *arg : argType) {
        funcTypeStr += arg->to_string() + ",";
    }
    if (!argType.empty())
        funcTypeStr.pop_back();
    funcTypeStr += ")";
    return funcTypeStr;
}

bool FunctionType::equal(const Type &other) const {
    if (other.getKind() != TK_Function) return false;
    const auto &ft = *llvm::cast<FunctionType>(&other);
    if (!retType->equal(*ft.getReturnType())) return false;
    if (argType.size() != ft.getArgTypes().size()) return false;
    for (size_t i = 0; i < argType.size(); ++i)
        if (!argType[i]->equal(*ft.getArgTypes()[i])) return false;
    return true;
}
