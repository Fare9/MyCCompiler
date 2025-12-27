
#include "mycc/Basic/TokenKinds.hpp"

using namespace mycc::tok;

static const char *const TokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "mycc/Basic/TokenKinds.def"
        nullptr // Last TokNames
};

const char *mycc::tok::getTokenName(TokenKind Kind)
{
    return TokNames[Kind];
}

const char *mycc::tok::getPunctuatorSpelling(TokenKind Kind)
{
    switch (Kind)
    {
#define PUNCTUATOR(ID, SP) \
    case ID:               \
        return SP;
#include "mycc/Basic/TokenKinds.def"
        default:
            break;
    }

    return nullptr;
}

const char *mycc::tok::getKeywordSpelling(TokenKind Kind)
{
    switch (Kind)
    {
#define KEYWORD(ID, FLAG) \
    case kw_##ID:         \
        return #ID;
#include "mycc/Basic/TokenKinds.def"
        default:
            break;
    }

    return nullptr;
}