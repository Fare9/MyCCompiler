#include "mycc/Basic/Diagnostic.hpp"

using namespace mycc;

// unnamed namespace to hold different variables
// that contain information about the errors
namespace {
    const char *DiagnosticText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "mycc/Basic/Diagnostic.def"
    };

    SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(ID, Level, Msg) SourceMgr::DK_##Level,
#include "mycc/Basic/Diagnostic.def"
    };

}

const char * DiagnosticsEngine::getDiagnosticText(unsigned DiagID)
{
    return DiagnosticText[DiagID];
}

SourceMgr::DiagKind DiagnosticsEngine::getDiagnosticKind(unsigned DiagID)
{
    return DiagnosticKind[DiagID];
}