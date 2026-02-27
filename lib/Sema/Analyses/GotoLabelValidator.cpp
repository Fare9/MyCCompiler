#include "mycc/Sema/Analyses/GotoLabelValidator.hpp"

using namespace mycc;

bool GotoLabelValidator::validateGotoLabels(DiagnosticsEngine & Diags) const {
    for (const auto &label: GotoLabels) {
        if (!FunctionLabels.contains(label)) {
            Diags.report(SMLoc(), diag::err_undefined_label, label.str());
            return true;
        }
    }
    return false;
}