#pragma once

#include "mycc/Basic/LLVM.hpp"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"



#include <fmt/format.h>


namespace mycc {

namespace diag {

/// @brief All the diagnostic levels and the error message
enum {
#define DIAG(ID, Level, Msg) ID,
#include "Diagnostic.def"
};
}

class DiagnosticsEngine
{
    /// @brief Obtain the error text
    /// @param DiagID id of the error
    /// @return error text
    static const char *getDiagnosticText(unsigned DiagID);

    /// @brief Obtain the Kind of Diagnostic given the ID
    /// @param DiagID id of the error
    /// @return kind of error
    static SourceMgr::DiagKind getDiagnosticKind(unsigned DiagID);

    /// @brief Source manager
    SourceMgr &SrcMgr;

    /// @brief Number of errors
    unsigned NumErrors;
public:
    explicit DiagnosticsEngine(SourceMgr &SrcMgr) : SrcMgr(SrcMgr), NumErrors(0) {}

    /// @brief Get the number of errors
    /// @return number of errors
    [[nodiscard]] unsigned numErrors() const
    {
        return NumErrors;
    }

    /// @brief Report an error on compilation time
    /// @tparam ...Args type of the arguments to show
    /// @param Loc location of the error
    /// @param DiagID ID of the error
    /// @param ...Arguments arguments to print with the error
    template <typename... Args>
    void report(SMLoc Loc, unsigned DiagID, Args &&... Arguments)
    {
        const char * diagnostic_text = getDiagnosticText(DiagID);
        std::string Msg = fmt::vformat(diagnostic_text, fmt::make_format_args(Arguments...));
        SourceMgr::DiagKind Kind = getDiagnosticKind(DiagID);
        SrcMgr.PrintMessage(Loc, Kind, Msg);
        NumErrors += (Kind == SourceMgr::DK_Error);
    }
};

}