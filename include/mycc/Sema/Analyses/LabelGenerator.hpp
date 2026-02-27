#pragma once

#include <string>

namespace mycc
{
    class LabelGenerator
    {
        // Counter for generating unique loop labels
        unsigned int LoopLabelCounter = 0;
        // Counter to generate Switch and Case labels
        unsigned int SwitchLabelCounter = 0;
        unsigned int CaseLabelCounter = 0;
        unsigned int DefaultLabelCounter = 0;
    public:
        LabelGenerator() = default;

        /**
         * @brief Generate a unique label for a loop construct.
         * @return Unique loop label string.
         */
        std::string generateLoopLabel();

        /**
         * @brief Generate a unique label for a switch statement.
         * @return Unique switch label string.
         */
        std::string generateSwitchLabel();

        /**
         * @brief Generate a unique label for a case statement.
         * @return Unique case label string.
         */
        std::string generateCaseLabel();

        /**
         * @brief Generate a unique label for a default statement.
         * @return Unique default label string.
         */
        std::string generateDefaultLabel();
    };
}