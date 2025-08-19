add_library(Basic
        TokenKinds.cpp
        Diagnostic.cpp)

target_link_libraries(Basic fmt::fmt)