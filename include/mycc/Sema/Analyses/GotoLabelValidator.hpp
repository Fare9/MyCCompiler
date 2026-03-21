#pragma once

#include <set>
#include "mycc/Basic/Diagnostic.hpp"

namespace mycc {
    class GotoLabelValidator {
        // Set that contains for a method the labels
        std::set<StringRef> FunctionLabels;
        // Set that contains all the jumped labels by Goto
        std::set<StringRef> GotoLabels;

    public:
        GotoLabelValidator() = default;

        void clearLabels() {
            FunctionLabels.clear();
            GotoLabels.clear();
        }

        void addFunctionLabel(const StringRef label) {
            FunctionLabels.insert(label);
        }

        [[nodiscard]] bool FunctionLabelContains(const StringRef label) const {
            return FunctionLabels.contains(label);
        }

        void addGotoLabel(const StringRef label) {
            GotoLabels.insert(label);
        }

        [[nodiscard]] bool GotoLabelContains(const StringRef label) const {
            return GotoLabels.contains(label);
        }

        bool validateGotoLabels(DiagnosticsEngine & Diags) const;
    };
}