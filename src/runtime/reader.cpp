#include "runtime/reader.hpp"

#include <cctype>
#include <cerrno>
#include <cstdlib>
#include <limits>
#include <sstream>
#include <stdexcept>

namespace goldfish::runtime {

namespace {

bool delimiter(char c) {
    return c == '\0' || std::isspace(static_cast<unsigned char>(c)) ||
           c == '(' || c == ')' || c == '[' || c == ']' || c == '\'' ||
           c == '`' || c == ',' ||
           c == ';' || c == '"';
}

bool decode_single_utf8(const std::string& text, char32_t& codepoint) {
    if (text.empty()) return false;
    const auto byte = [&text](std::size_t index) {
        return static_cast<unsigned char>(text[index]);
    };
    const unsigned char first = byte(0);
    std::size_t length = 0;
    char32_t value = 0;
    if (first <= 0x7f) {
        length = 1;
        value = first;
    } else if (first >= 0xc2 && first <= 0xdf) {
        length = 2;
        value = first & 0x1f;
    } else if (first >= 0xe0 && first <= 0xef) {
        length = 3;
        value = first & 0x0f;
    } else if (first >= 0xf0 && first <= 0xf4) {
        length = 4;
        value = first & 0x07;
    } else {
        return false;
    }
    if (text.size() != length) return false;
    for (std::size_t i = 1; i < length; ++i) {
        const unsigned char continuation = byte(i);
        if ((continuation & 0xc0) != 0x80) return false;
        value = (value << 6) | (continuation & 0x3f);
    }
    if ((length == 3 && value < 0x800) ||
        (length == 4 && value < 0x10000) ||
        (value >= 0xd800 && value <= 0xdfff) || value > 0x10ffff)
        return false;
    codepoint = value;
    return true;
}

void append_utf8(std::string& text, char32_t codepoint) {
    if (codepoint <= 0x7f) {
        text.push_back(static_cast<char>(codepoint));
    } else if (codepoint <= 0x7ff) {
        text.push_back(static_cast<char>(0xc0 | (codepoint >> 6)));
        text.push_back(static_cast<char>(0x80 | (codepoint & 0x3f)));
    } else if (codepoint <= 0xffff) {
        text.push_back(static_cast<char>(0xe0 | (codepoint >> 12)));
        text.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
        text.push_back(static_cast<char>(0x80 | (codepoint & 0x3f)));
    } else {
        text.push_back(static_cast<char>(0xf0 | (codepoint >> 18)));
        text.push_back(static_cast<char>(0x80 | ((codepoint >> 12) & 0x3f)));
        text.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
        text.push_back(static_cast<char>(0x80 | (codepoint & 0x3f)));
    }
}

} // namespace

char TinyReader::peek() const {
    return position_ == source_.size() ? '\0' : source_[position_];
}

char TinyReader::next() {
    if (position_ == source_.size())
        return '\0';
    return source_[position_++];
}

bool TinyReader::consume(char expected) {
    if (peek() != expected)
        return false;
    ++position_;
    return true;
}

[[noreturn]] void TinyReader::error(const std::string& message) const {
    std::ostringstream detail;
    detail << "tiny reader: " << message << " at byte " << position_;
    throw std::runtime_error(detail.str());
}

void TinyReader::skip_space() {
    while (true) {
        while (std::isspace(static_cast<unsigned char>(peek())))
            next();
        if (peek() != ';')
            return;
        while (peek() != '\0' && next() != '\n')
            continue;
    }
}

std::optional<Value> TinyReader::read() {
    skip_space();
    if (peek() == '\0')
        return std::nullopt;
    return read_form();
}

Value TinyReader::read_form() {
    skip_space();
    switch (peek()) {
    case '(':
        return read_list(')');
    case '[':
        return read_list(']');
    case '\'':
        next();
        return evaluator_.list({evaluator_.symbol("quote"), read_form()});
    case '`':
        next();
        return evaluator_.list({evaluator_.symbol("quasiquote"), read_form()});
    case ',': {
        next();
        const char* name = consume('@') ? "unquote-splicing" : "unquote";
        return evaluator_.list({evaluator_.symbol(name), read_form()});
    }
    case '"':
        return read_string();
    case '|':
        return read_quoted_symbol();
    case '#':
        return read_dispatch();
    case ')':
        error("unexpected ')'" );
    default:
        return read_atom();
    }
}

Value TinyReader::read_dispatch() {
    if (position_ + 1 < source_.size() && source_[position_ + 1] == '(')
        return read_vector();
    if (position_ + 1 < source_.size() && source_[position_ + 1] == '\\')
        return read_character();

    std::size_t saved = position_;
    next();
    if (consume('\''))
        return evaluator_.list({evaluator_.symbol("syntax"), read_form()});
    if (consume('`'))
        return evaluator_.list({evaluator_.symbol("quasisyntax"), read_form()});
    if (consume(',')) {
        const char* name = consume('@') ? "unsyntax-splicing" : "unsyntax";
        return evaluator_.list({evaluator_.symbol(name), read_form()});
    }
    if (!std::isdigit(static_cast<unsigned char>(peek()))) {
        position_ = saved;
        return read_atom();
    }
    std::size_t label = 0;
    while (std::isdigit(static_cast<unsigned char>(peek()))) {
        const std::size_t digit = static_cast<std::size_t>(next() - '0');
        if (label > (std::numeric_limits<std::size_t>::max() - digit) / 10)
            error("datum label is too large");
        label = label * 10 + digit;
    }
    if (consume('=')) {
        if (labels_.count(label))
            error("duplicate datum label");
        Value value = read_form();
        labels_.emplace(label, value);
        return value;
    }
    if (consume('#')) {
        auto found = labels_.find(label);
        if (found == labels_.end())
            error("unknown datum label");
        return found->second;
    }
    position_ = saved;
    return read_atom();
}

Value TinyReader::read_vector() {
    next();
    if (!consume('('))
        error("malformed vector");
    std::vector<Value> values;
    skip_space();
    while (!consume(')')) {
        if (peek() == '\0')
            error("unterminated vector");
        values.push_back(read_form());
        skip_space();
    }
    return evaluator_.vector(values);
}

Value TinyReader::read_character() {
    next();
    next();
    std::string token;
    while (!delimiter(peek()))
        token.push_back(next());
    if (token.empty()) {
        char character = next();
        if (character == '\0')
            error("missing character literal");
        return evaluator_.character(static_cast<unsigned char>(character));
    }
    if (token == "space")
        return evaluator_.character(U' ');
    if (token == "newline")
        return evaluator_.character(U'\n');
    if (token == "tab")
        return evaluator_.character(U'\t');
    if (token == "return")
        return evaluator_.character(U'\r');
    if (token == "null")
        return evaluator_.character(U'\0');
    if (token == "alarm")
        return evaluator_.character(U'\a');
    // s7's serialized character spelling for form feed is `#\\xc`.
    if (token == "xc")
        return evaluator_.character(U'\f');
    if (token == "backspace")
        return evaluator_.character(U'\b');
    if (token == "escape")
        return evaluator_.character(U'\x1b');
    if (token == "delete")
        return evaluator_.character(U'\x7f');
    if (token.size() > 1 && token[0] == 'x') {
        char* end = nullptr;
        unsigned long codepoint = std::strtoul(token.c_str() + 1, &end, 16);
        if (end == token.c_str() + 1 || *end != '\0' || codepoint > 0x10ffff)
            error("invalid hexadecimal character literal: " + token);
        return evaluator_.character(static_cast<char32_t>(codepoint));
    }
    char32_t codepoint = 0;
    if (decode_single_utf8(token, codepoint))
        return evaluator_.character(codepoint);
    if (token.size() != 1)
        error("unsupported character literal: " + token);
    return evaluator_.character(static_cast<unsigned char>(token[0]));
}

Value TinyReader::read_list(char closing) {
    next();
    std::vector<Value> values;
    skip_space();
    if (consume(closing))
        return Value::null();

    while (true) {
        if (peek() == '\0')
            error("unterminated list");
        if (peek() == '.' &&
            delimiter(position_ + 1 < source_.size() ? source_[position_ + 1]
                                                       : '\0')) {
            const std::size_t dot_position = position_;
            next();
            if (values.empty())
                error(" dotted pair has no head");
            Value tail = read_form();
            skip_space();
            if (!consume(closing))
                error(std::string("dotted pair expected '") + closing +
                      "' after its tail, found byte " +
                      std::to_string(static_cast<unsigned char>(peek())) +
                      " (dot at byte " + std::to_string(dot_position) + ")");
            for (auto it = values.rbegin(); it != values.rend(); ++it)
                tail = evaluator_.pair(*it, tail);
            return tail;
        }
        values.push_back(read_form());
        skip_space();
        if (consume(closing))
            break;
    }
    return evaluator_.list(values);
}

std::string TinyReader::read_escaped_text(char closing) {
    next();
    std::string value;
    while (true) {
        char c = next();
        if (c == '\0')
            error("unterminated string");
        if (c == closing) {
            return value;
        }
        if (c != '\\') {
            value.push_back(c);
            continue;
        }
        char escaped = next();
        switch (escaped) {
        case 'a': value.push_back('\a'); break;
        case 'b': value.push_back('\b'); break;
        case 'n': value.push_back('\n'); break;
        case 'r': value.push_back('\r'); break;
        case 't': value.push_back('\t'); break;
        case 'f': value.push_back('\f'); break;
        case 'v': value.push_back('\v'); break;
        case '0': value.push_back('\0'); break;
        case 'e': value.push_back('\x1b'); break;
        case '\\': value.push_back('\\'); break;
        case '"': value.push_back('"'); break;
        case '|': value.push_back('|'); break;
        case 'x': {
            char32_t codepoint = 0;
            std::size_t digits = 0;
            while (true) {
                const unsigned char digit_char =
                    static_cast<unsigned char>(peek());
                unsigned digit = digit_char >= '0' && digit_char <= '9'
                    ? digit_char - '0'
                    : digit_char >= 'a' && digit_char <= 'f'
                        ? digit_char - 'a' + 10
                        : digit_char >= 'A' && digit_char <= 'F'
                            ? digit_char - 'A' + 10 : 16;
                if (digit == 16) break;
                if (codepoint > (0x10ffff - digit) / 16)
                    error("hexadecimal string escape is out of range");
                codepoint = codepoint * 16 + digit;
                ++digits;
                next();
            }
            if (digits == 0 || !consume(';') ||
                (codepoint >= 0xd800 && codepoint <= 0xdfff))
                error("invalid hexadecimal string escape");
            append_utf8(value, codepoint);
            break;
        }
        default:
            // S7's writer escapes punctuation in serialized Scheme text.
            // Keep that cache vocabulary readable while still rejecting
            // unknown alphabetic escapes that could hide source mistakes.
            if (!std::isalnum(static_cast<unsigned char>(escaped)))
                value.push_back(escaped);
            else
                error(std::string("unsupported string escape \\") + escaped);
        }
    }
}

Value TinyReader::read_string() {
    return evaluator_.string(read_escaped_text('"'));
}

Value TinyReader::read_quoted_symbol() {
    return evaluator_.symbol(read_escaped_text('|'));
}

Value TinyReader::read_atom() {
    std::string token;
    while (!delimiter(peek()))
        token.push_back(next());
    if (token.empty())
        error("empty token");
    if (token == "#t")
        return Value::boolean(true);
    if (token == "#f")
        return Value::boolean(false);

    Number number;
    if (parse_number(token, number))
        return evaluator_.number(std::move(number));
    return evaluator_.symbol(token);
}

} // namespace goldfish::runtime
