#pragma once

#include <cstdint>

namespace goldfish::runtime {

std::uint32_t unicode_char_upcase(std::uint32_t codepoint);
std::uint32_t unicode_char_downcase(std::uint32_t codepoint);
bool unicode_char_alphabetic(std::uint32_t codepoint);
bool unicode_char_upper_case(std::uint32_t codepoint);
bool unicode_char_lower_case(std::uint32_t codepoint);
bool unicode_char_numeric(std::uint32_t codepoint);
bool unicode_char_whitespace(std::uint32_t codepoint);
std::uint32_t unicode_char_foldcase(std::uint32_t codepoint);

} // namespace goldfish::runtime
