#pragma once
#include <string>

namespace mycc
{
    /**
        * @brief We keep this structure to maintain the context for the break instructions.
        * with the `base_level` we generate the break and the continue label. We also
        * keep metadata to know if we are inside a loop.
        */
    struct BreakableContext {
        std::string base_label;
        bool is_loop = false; // metadata, it contains true for loops, and false for switches

        [[nodiscard]] std::string get_break_label() const {
            return base_label + "_end";
        }

        [[nodiscard]] std::string get_continue_label() const {
            return base_label + "_continue";
        }
    };
}
