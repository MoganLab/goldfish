#pragma once

#include <cstddef>
#include <stdexcept>
#include <string>

namespace goldfish::runtime {

inline bool utf8_decode_at(const std::string& text, std::size_t position,
                           char32_t& codepoint, std::size_t& width) {
    if (position >= text.size()) return false;
    const auto first = static_cast<unsigned char>(text[position]);
    if (first <= 0x7f) { width = 1; codepoint = first; }
    else if (first >= 0xc2 && first <= 0xdf) { width = 2; codepoint = first & 0x1f; }
    else if (first >= 0xe0 && first <= 0xef) { width = 3; codepoint = first & 0x0f; }
    else if (first >= 0xf0 && first <= 0xf4) { width = 4; codepoint = first & 0x07; }
    else return false;
    if (width > text.size() - position) return false;
    for (std::size_t i = 1; i < width; ++i) {
        const auto byte = static_cast<unsigned char>(text[position + i]);
        if ((byte & 0xc0) != 0x80) return false;
        codepoint = (codepoint << 6) | (byte & 0x3f);
    }
    return !((width == 3 && codepoint < 0x800) ||
             (width == 4 && codepoint < 0x10000) ||
             (codepoint >= 0xd800 && codepoint <= 0xdfff) || codepoint > 0x10ffff);
}

inline char32_t utf8_character_at(const std::string& text, std::size_t position,
                                 std::size_t& width) {
    char32_t codepoint = 0;
    if (!utf8_decode_at(text, position, codepoint, width))
        throw std::runtime_error("value-error: invalid UTF-8");
    return codepoint;
}

inline std::size_t utf8_length(const std::string& text) {
    std::size_t count = 0;
    for (std::size_t position = 0; position < text.size(); ++count) {
        std::size_t width = 0;
        utf8_character_at(text, position, width);
        position += width;
    }
    return count;
}

// Character indexes may point one past the last character for empty slices.
inline std::size_t utf8_byte_offset(const std::string& text, std::size_t index) {
    std::size_t position = 0;
    for (std::size_t i = 0; i < index; ++i) {
        if (position == text.size())
            throw std::runtime_error("out-of-range: string index out of bounds");
        std::size_t width = 0;
        utf8_character_at(text, position, width);
        position += width;
    }
    return position;
}

} // namespace goldfish::runtime
