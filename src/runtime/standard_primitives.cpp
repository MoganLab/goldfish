#include "runtime/standard_primitives.hpp"

#include "runtime/debug_flags.hpp"
#include "runtime/reader.hpp"
#include "runtime/platform_primitives.hpp"
#include "runtime/unicode_primitives.hpp"
#include "runtime/utf8.hpp"

#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <complex>
#include <cstring>
#include <functional>
#include <cstdlib>
#include <fstream>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <limits>
#include <memory>
#include <numeric>
#include <optional>
#include <sstream>
#include <stdexcept>

namespace goldfish::runtime {

namespace fs = std::filesystem;

namespace {

void require_arity(const Values& args, std::size_t count,
                   const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments, got " +
                                 std::to_string(args.size()));
}

const std::vector<Value>& vector_storage(Value value) {
    if (!value.is_object() ||
        value.as_object()->type() != ObjectType::Vector)
        throw std::runtime_error("expected vector");
    return value.as_object<VectorObject>()->values;
}

// Per-site key override: some host tests pin a key the message
// classifier would not give (file path ops want 'type-error while
// string_value elsewhere pins 'wrong-type-arg for the same shape).
[[noreturn]] void raise_keyed(Evaluator& evaluator, const char* key,
                              const std::string& message) {
    throw RaisedValue(Value::object(evaluator.heap().make<ErrorObject>(
        message, ValueList{evaluator.string(message)}, key)));
}

bool same(Value left, Value right) {
    if (left == right) return true;
    // Characters are immediate on the host: eq?/eqv? compare codepoints, not
    // object identity.  The Scheme reader's `case ch' dispatch (liii reader)
    // depends on this -- identity comparison made every character literal
    // miss and warm reads fell through to read-symbol.
    if (left.is_object() && right.is_object() &&
        left.as_object()->type() == ObjectType::Character &&
        right.as_object()->type() == ObjectType::Character)
        return left.as_object<CharacterObject>()->value ==
               right.as_object<CharacterObject>()->value;
    if (left.is_object() && right.is_object() &&
        left.as_object()->type() == ObjectType::Eof &&
        right.as_object()->type() == ObjectType::Eof)
        return true;
    return false;
}

bool symbol_named(Value value, const char* name) {
    return value.is_object() &&
           value.as_object()->type() == ObjectType::Symbol &&
           value.as_object<SymbolObject>()->name == name;
}

Values port_parameter(Value& slot, const Values& args, ObjectType type,
                      const char* name) {
    if (args.empty()) return Values{slot};
    const bool convert = args.size() == 2 &&
                         symbol_named(args[1], "%parameter-convert");
    const bool exchange = args.size() == 2 &&
                          symbol_named(args[1], "%parameter-exchange");
    if (args.size() > 2 || (args.size() == 2 && !convert && !exchange))
        throw std::runtime_error(std::string(name) + ": invalid parameter arguments");
    if (!args[0].is_object() || args[0].as_object()->type() != type)
        throw std::runtime_error(std::string(name) + ": expected a matching port");
    if (convert) return Values{args[0]};
    Value old = slot;
    slot = args[0];
    return Values{exchange ? old : Value::unspecified()};
}

bool integer_value(Value value, BigInteger& result) {
    if (!is_number(value)) return false;
    Number number = number_value(value);
    if (!number.is_real() || !number.real.is_integer()) return false;
    if (number.real.inexact) {
        if (!std::isfinite(number.real.inexact_value)) return false;
        number.real = exact_from_double(number.real.inexact_value);
    }
    result = number.real.numerator;
    return true;
}

BigInteger exact_integer(Value value, const char* name) {
    if (value.is_integer()) return BigInteger(value.as_integer());
    if (is_number(value)) {
        const Number number = number_value(value);
        if (number.is_exact() && number.is_integer())
            return number.real.numerator;
    }
    throw std::runtime_error(std::string(name) + " expects exact integers");
}

unsigned population_count(std::uint64_t value) {
    value -= (value >> 1) & 0x5555555555555555ULL;
    value = (value & 0x3333333333333333ULL) +
            ((value >> 2) & 0x3333333333333333ULL);
    value = (value + (value >> 4)) & 0x0f0f0f0f0f0f0f0fULL;
    return static_cast<unsigned>((value * 0x0101010101010101ULL) >> 56);
}

BigInteger truncate_real_quotient(const Number& dividend,
                                 const Number& divisor) {
    Number ratio = number_divide(dividend, divisor);
    RealNumber real = ratio.real;
    if (real.inexact) {
        if (!std::isfinite(real.inexact_value))
            throw std::runtime_error("quotient result is not finite");
        real = exact_from_double(real.inexact_value);
    }
    return real.numerator / real.denominator;
}

Value make_integer(Evaluator& evaluator, BigInteger value,
                   bool inexact = false) {
    if (inexact)
        return evaluator.number(Number::inexact(value.to_double()));
    return evaluator.number(Number::exact(std::move(value)));
}

BigInteger exact_floor(const RealNumber& value) {
    BigInteger quotient = value.numerator / value.denominator;
    if (value.numerator.negative() &&
        !(value.numerator % value.denominator).is_zero())
        quotient -= BigInteger(1);
    return quotient;
}

RealNumber rational_add(const RealNumber& a, const RealNumber& b) {
    return RealNumber::exact(a.numerator * b.denominator +
                             b.numerator * a.denominator,
                             a.denominator * b.denominator);
}

RealNumber rational_subtract(const RealNumber& a, const RealNumber& b) {
    return RealNumber::exact(a.numerator * b.denominator -
                             b.numerator * a.denominator,
                             a.denominator * b.denominator);
}

RealNumber rational_negate(const RealNumber& value) {
    return RealNumber::exact(-value.numerator, value.denominator);
}

RealNumber simplest_rational(RealNumber lower, RealNumber upper) {
    const RealNumber zero = RealNumber::exact(BigInteger(0));
    if (compare(lower, zero) <= 0 && compare(upper, zero) >= 0)
        return zero;
    if (compare(upper, zero) < 0) {
        RealNumber result = simplest_rational(rational_negate(upper),
                                              rational_negate(lower));
        return rational_negate(result);
    }

    BigInteger low_floor = exact_floor(lower);
    BigInteger high_floor = exact_floor(upper);
    if (low_floor != high_floor)
        return RealNumber::exact(low_floor + BigInteger(1));
    if (lower.denominator == BigInteger(1)) return lower;

    RealNumber whole = RealNumber::exact(low_floor);
    RealNumber low_fraction = rational_subtract(lower, whole);
    RealNumber high_fraction = rational_subtract(upper, whole);
    RealNumber reciprocal_low = RealNumber::exact(
        high_fraction.denominator, high_fraction.numerator);
    RealNumber reciprocal_high = RealNumber::exact(
        low_fraction.denominator, low_fraction.numerator);
    RealNumber tail = simplest_rational(std::move(reciprocal_low),
                                        std::move(reciprocal_high));
    RealNumber reciprocal_tail = RealNumber::exact(tail.denominator,
                                                   tail.numerator);
    return rational_add(whole, reciprocal_tail);
}

std::string value_hint(Value value, int depth = 0) {
    if (depth >= 4) return "...";
    if (value.is_integer()) return std::to_string(value.as_integer());
    if (value.is_boolean()) return value.as_boolean() ? "#t" : "#f";
    if (value.is_null()) return "()";
    if (is_number(value)) return number_to_string(value);
    if (!value.is_object()) return "#<value>";
    switch (value.as_object()->type()) {
    case ObjectType::Symbol:
        return value.as_object<SymbolObject>()->name;
    case ObjectType::String:
        return "\"" + value.as_object<StringObject>()->value + "\"";
    case ObjectType::Pair: {
        std::string result = "(";
        Value rest = value;
        for (int i = 0; i < 4 && rest.is_object() &&
             rest.as_object()->type() == ObjectType::Pair; ++i) {
            if (i) result += " ";
            auto* pair = rest.as_object<PairObject>();
            result += value_hint(pair->car, depth + 1);
            rest = pair->cdr;
        }
        if (!rest.is_null()) result += " ...";
        return result + ")";
    }
    case ObjectType::Vector: {
        std::string result = "#(";
        const auto& values = value.as_object<VectorObject>()->values;
        std::size_t limit = std::min<std::size_t>(values.size(), 5);
        for (std::size_t i = 0; i < limit; ++i) {
            if (i) result += " ";
            result += value_hint(values[i], depth + 1);
        }
        if (values.size() > limit) result += " ...";
        return result + ")";
    }
    default:
        return "#<object:" + std::to_string(
            static_cast<unsigned>(value.as_object()->type())) + ">";
    }
}

std::string raised_message(const RaisedValue& raised) {
    Value value = raised.value();
    if (value.is_object() &&
        value.as_object()->type() == ObjectType::ErrorObject) {
        const auto* error = value.as_object<ErrorObject>();
        std::string message = error->message;
        for (Value irritant : error->irritants) {
            message += " ";
            if (irritant.is_integer())
                message += std::to_string(irritant.as_integer());
            else if (irritant.is_object() &&
                     irritant.as_object()->type() == ObjectType::String)
                message += irritant.as_object<StringObject>()->value;
            else if (irritant.is_object() &&
                     irritant.as_object()->type() == ObjectType::Symbol)
                message += irritant.as_object<SymbolObject>()->name;
            else
                message += value_hint(irritant);
        }
        return message;
    }
    return "raised Scheme value";
}

std::vector<Value> proper_list(Value value) {
    if (!is_proper_list(value))
        throw std::runtime_error("expected proper list");
    std::vector<Value> result;
    while (!value.is_null()) {
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("expected proper list");
        auto* pair = value.as_object<PairObject>();
        result.push_back(pair->car);
        value = pair->cdr;
    }
    return result;
}

namespace {
// Cycle guard for deep equality: comparing two cyclic aggregate graphs
// (library records reference their own bindings' homes) would otherwise
// recurse forever.  Revisiting a (left, right) pair on the current path
// means the structures agree so far -- assume equal, like s7/Racket do.
bool equal_inner(Value left, Value right,
                 std::vector<std::pair<const Object*, const Object*>>& seen) {
    if (is_number(left) && is_number(right))
        return equivalent(left, right);
    if (left == right)
        return true;
    if (!left.is_object() || !right.is_object() ||
        left.as_object()->type() != right.as_object()->type())
        return false;
    switch (left.as_object()->type()) {
    case ObjectType::Pair: {
        PairObject* a = left.as_object<PairObject>();
        PairObject* b = right.as_object<PairObject>();
        const Object* key_left = a;
        const Object* key_right = b;
        for (const auto& entry : seen)
            if (entry.first == key_left && entry.second == key_right)
                return true;
        seen.emplace_back(key_left, key_right);
        bool result = equal_inner(a->car, b->car, seen) &&
                      equal_inner(a->cdr, b->cdr, seen);
        seen.pop_back();
        return result;
    }
    case ObjectType::String:
        return left.as_object<StringObject>()->value ==
               right.as_object<StringObject>()->value;
    case ObjectType::Bytevector:
        return left.as_object<BytevectorObject>()->bytes ==
               right.as_object<BytevectorObject>()->bytes;
    case ObjectType::Character:
        return left.as_object<CharacterObject>()->value ==
               right.as_object<CharacterObject>()->value;
    case ObjectType::Eof:
        return true;
    case ObjectType::Vector: {
        VectorObject* a = left.as_object<VectorObject>();
        VectorObject* b = right.as_object<VectorObject>();
        if (a->values.size() != b->values.size())
            return false;
        for (const auto& entry : seen)
            if (entry.first == a && entry.second == b)
                return true;
        seen.emplace_back(a, b);
        bool result = true;
        for (std::size_t i = 0; i < a->values.size() && result; ++i)
            result = equal_inner(a->values[i], b->values[i], seen);
        seen.pop_back();
        return result;
    }
    default:
        return false;
    }
}
} // namespace

bool equal_values_impl(Value left, Value right) {
    std::vector<std::pair<const Object*, const Object*>> seen;
    return equal_inner(left, right, seen);
}

InputStringPortObject& input_port(Value value, const char* name) {
    if (!value.is_object() || value.as_object()->type() != ObjectType::InputPort)
        throw std::runtime_error(std::string(name) + " expects an input port");
    auto& port = *value.as_object<InputStringPortObject>();
    if (port.closed)
        throw std::runtime_error(std::string(name) + " from closed input port");
    return port;
}

bool source_delimiter(unsigned char c) {
    // `.' is NOT a delimiter: tokens like `...', `foo.bar' and `1.5' must
    // read whole.  Standalone-dot (dotted pair) detection happens in the
    // caller via delimiter?(next char), as reader.cpp does.
    return std::isspace(c) || c == '(' || c == ')' || c == '[' || c == ']' ||
           c == '"' || c == ';';
}

void append_utf8(std::string& output, unsigned value) {
    if (value <= 0x7f) output.push_back(static_cast<char>(value));
    else if (value <= 0x7ff) {
        output.push_back(static_cast<char>(0xc0 | (value >> 6)));
        output.push_back(static_cast<char>(0x80 | (value & 0x3f)));
    } else if (value <= 0xffff) {
        output.push_back(static_cast<char>(0xe0 | (value >> 12)));
        output.push_back(static_cast<char>(0x80 | ((value >> 6) & 0x3f)));
        output.push_back(static_cast<char>(0x80 | (value & 0x3f)));
    } else {
        output.push_back(static_cast<char>(0xf0 | (value >> 18)));
        output.push_back(static_cast<char>(0x80 | ((value >> 12) & 0x3f)));
        output.push_back(static_cast<char>(0x80 | ((value >> 6) & 0x3f)));
        output.push_back(static_cast<char>(0x80 | (value & 0x3f)));
    }
}

std::string utf8_encode_char(char32_t codepoint) {
    if ((codepoint >= 0xd800 && codepoint <= 0xdfff) || codepoint > 0x10ffff)
        throw std::runtime_error("value-error: invalid Unicode scalar value");
    std::string out;
    if (codepoint < 0x80) {
        out.push_back(static_cast<char>(codepoint));
    } else if (codepoint < 0x800) {
        out.push_back(static_cast<char>(0xc0 | (codepoint >> 6)));
        out.push_back(static_cast<char>(0x80 | (codepoint & 0x3f)));
    } else if (codepoint < 0x10000) {
        out.push_back(static_cast<char>(0xe0 | (codepoint >> 12)));
        out.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
        out.push_back(static_cast<char>(0x80 | (codepoint & 0x3f)));
    } else {
        out.push_back(static_cast<char>(0xf0 | (codepoint >> 18)));
        out.push_back(static_cast<char>(0x80 | ((codepoint >> 12) & 0x3f)));
        out.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
        out.push_back(static_cast<char>(0x80 | (codepoint & 0x3f)));
    }
    return out;
}

bool utf8_width_at(const std::string& text, std::size_t position,
                   std::size_t& decoded_width) {
    char32_t codepoint = 0;
    return utf8_decode_at(text, position, codepoint, decoded_width);
}

bool utf8_offsets(const std::string& text, std::vector<std::size_t>& offsets) {
    offsets.clear();
    offsets.push_back(0);
    std::size_t position = 0;
    while (position < text.size()) {
        std::size_t width = 0;
        if (!utf8_width_at(text, position, width)) return false;
        position += width;
        offsets.push_back(position);
    }
    return true;
}

bool utf8_valid(const std::string& text) {
    std::size_t position = 0;
    while (position < text.size()) {
        if (static_cast<unsigned char>(text[position]) < 0x80) {
            ++position;
            continue;
        }
        std::size_t width = 0;
        if (!utf8_width_at(text, position, width)) return false;
        position += width;
    }
    return true;
}

// write-form of a character: #\newline/#\space/#\tab/#\return by name, raw
// for printable ASCII and non-ASCII, #\xN (unpadded, as s7 writes it) for
// the remaining control codepoints.
std::string character_literal(char32_t codepoint) {
    switch (codepoint) {
        case U'\n': return "#\\newline";
        case U' ': return "#\\space";
        case U'\t': return "#\\tab";
        case U'\r': return "#\\return";
        default: break;
    }
    if (codepoint >= 0x20 && codepoint < 0x7f)
        return std::string("#\\") + static_cast<char>(codepoint);
    if (codepoint >= 0x7f) return "#\\" + utf8_encode_char(codepoint);
    static constexpr char digits[] = "0123456789abcdef";
    std::string out = "#\\x";
    if (codepoint == 0) out += '0';
    else {
        char buffer[8];
        int index = 0;
        while (codepoint) {
            buffer[index++] = digits[codepoint & 0xf];
            codepoint >>= 4;
        }
        while (index) out += buffer[--index];
    }
    return out;
}

std::string symbol_literal(const std::string& name) {
    Number numeric;
    const auto initial = [](unsigned char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
               (c != 0 && std::strchr("!$%&*/:<=>?^_~", c));
    };
    const auto sign_subsequent = [&](unsigned char c) {
        return initial(c) || c == '+' || c == '-' || c == '@';
    };
    const auto dot_subsequent = [&](unsigned char c) {
        return sign_subsequent(c) || c == '.';
    };
    bool valid_start = !name.empty() && initial(name[0]);
    if (!name.empty() && (name[0] == '+' || name[0] == '-'))
        valid_start = name.size() == 1 || sign_subsequent(name[1]) ||
            (name.size() > 2 && name[1] == '.' && dot_subsequent(name[2]));
    if (!name.empty() && name[0] == '.')
        valid_start = name.size() > 1 && dot_subsequent(name[1]);
    const bool quoted = !valid_start ||
        parse_number(name, numeric) ||
        std::any_of(name.begin(), name.end(), [](unsigned char c) {
            return !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                     (c >= '0' && c <= '9') ||
                     (c != 0 && std::strchr("!$%&*/:<=>?^_~+-.@", c)));
        });
    if (!quoted) return name;
    std::string out = "|";
    constexpr char hex[] = "0123456789abcdef";
    for (unsigned char c : name) {
        if (c == '|' || c == '\\') out += '\\';
        if (c < 0x20 || c == 0x7f) {
            out += "\\x";
            out += hex[c >> 4];
            out += hex[c & 15];
            out += ';';
        } else out += static_cast<char>(c);
    }
    return out + "|";
}

// Atomic representations for the Scheme graph writer and engine diagnostics.
std::string format_value(const Evaluator& evaluator, Value value,
                         bool write_mode, int depth) {
    if (depth > 200) return "#<deep>";
    if (value.is_unspecified()) return "#<unspecified>";
    if (value.is_integer()) return std::to_string(value.as_integer());
    if (is_number(value)) return number_to_string(value);
    if (value.is_boolean()) return value.as_boolean() ? "#t" : "#f";
    if (value.is_null()) return "()";
    if (!value.is_object()) return "#<object>";
    switch (value.as_object()->type()) {
        case ObjectType::String: {
            const std::string& text = value.as_object<StringObject>()->value;
            if (!write_mode) return text;
            std::string quoted = "\"";
            constexpr char hex[] = "0123456789abcdef";
            for (unsigned char character : text) {
                switch (character) {
                case '"': quoted += "\\\""; break;
                case '\\': quoted += "\\\\"; break;
                case '\a': quoted += "\\a"; break;
                case '\b': quoted += "\\b"; break;
                case '\t': quoted += "\\t"; break;
                case '\n': quoted += "\\n"; break;
                case '\r': quoted += "\\r"; break;
                default:
                    if (character < 0x20 || character == 0x7f) {
                        quoted += "\\x";
                        quoted.push_back(hex[character >> 4]);
                        quoted.push_back(hex[character & 0x0f]);
                        quoted.push_back(';');
                    } else {
                        quoted.push_back(static_cast<char>(character));
                    }
                }
            }
            quoted.push_back('"');
            return quoted;
        }
        case ObjectType::Number:
            return number_to_string(value);
        case ObjectType::Symbol:
            return write_mode ? symbol_literal(value.as_object<SymbolObject>()->name)
                              : value.as_object<SymbolObject>()->name;
        case ObjectType::Character: {
            const char32_t codepoint = value.as_object<CharacterObject>()->value;
            if (write_mode) return character_literal(codepoint);
            return utf8_encode_char(codepoint);
        }
        case ObjectType::Eof:
            return "#<eof>";
        case ObjectType::Bytevector: {
            std::string out = "#u8(";
            const auto& bytes = value.as_object<BytevectorObject>()->bytes;
            for (std::size_t i = 0; i < bytes.size(); ++i) {
                if (i) out += ' ';
                out += std::to_string(static_cast<unsigned char>(bytes[i]));
            }
            return out + ')';
        }
        case ObjectType::ErrorObject:
            return "#<error " + value.as_object<ErrorObject>()->message + ">";
        case ObjectType::Closure:
        case ObjectType::Primitive:
        case ObjectType::Continuation:
            return "#<procedure>";
        case ObjectType::Vector: {
            std::string out = "#(";
            const auto& items = value.as_object<VectorObject>()->values;
            for (std::size_t i = 0; i < items.size(); ++i) {
                if (i) out += " ";
                out += format_value(evaluator, items[i], true, depth + 1);
            }
            return out + ")";
        }
        case ObjectType::Pair: {
            // Proper lists print space-separated like the host; only a
            // dotted tail shows the cons explicitly.
            std::string out = "(";
            Value rest = value;
            bool first = true;
            bool dotted = false;
            while (rest.is_object() &&
                   rest.as_object()->type() == ObjectType::Pair) {
                if (!first) out += " ";
                first = false;
                const auto* pair = rest.as_object<PairObject>();
                out += format_value(evaluator, pair->car, true, depth + 1);
                rest = pair->cdr;
            }
            if (!rest.is_null()) {
                if (!first) out += " . ";
                out += format_value(evaluator, rest, true, depth + 1);
                dotted = true;
            }
            (void)dotted;
            return out + ")";
        }
        default:
            return "#<object>";
    }
}

std::string format_value(const Evaluator& evaluator, Value value) {
    return format_value(evaluator, value, false, 0);
}

// The Scheme-visible current ports.  with-input-from-file /
// with-output-to-file rebind them around a thunk, so every writer consults
// the slot at call time instead of capturing the stdout port.
struct CurrentPortSlot {
    Value input;
    Value output;
    Value error_port;
};
CurrentPortSlot g_current_ports;
// The eof singleton primitives close over; a permanent root keeps the
// captured copies alive across collection.
Value g_eof_singleton;
// Registered as permanent roots: primitives close over these values and
// set! reassigns the slots, so rooting the slots covers both.
struct RegisterPortRoots {
    RegisterPortRoots() {
        permanent_roots().push_back(&g_current_ports.input);
        permanent_roots().push_back(&g_current_ports.output);
        permanent_roots().push_back(&g_current_ports.error_port);
        permanent_roots().push_back(&g_eof_singleton);
    }
};
RegisterPortRoots g_register_port_roots;

Value current_input_port(Evaluator& evaluator) {
    if (g_current_ports.input.is_null())
        g_current_ports.input = Value::object(
            evaluator.heap().make<InputStringPortObject>(std::string()));
    return g_current_ports.input;
}

void install(Evaluator& evaluator, const char* name,
             PrimitiveObject::Function function) {
    evaluator.define_primitive(name, std::move(function));
}

OutputPortObject& output_port(Value value, const char* name) {
    if (!value.is_object() || value.as_object()->type() != ObjectType::OutputPort)
        throw std::runtime_error(std::string(name) + " expects an output port");
    auto& port = *value.as_object<OutputPortObject>();
    if (port.closed)
        throw std::runtime_error(std::string(name) + " on closed output port");
    return port;
}

// (copy obj) / (copy src dest [start end]) -- s7's copy.  One argument is a
// SHALLOW copy: a fresh container that shares its elements (the contract in
// tests/liii/base/copy-test pins the sharing).  With a target, src[start,end)
// fills dest from index 0 and dest comes back.
Value copy_value(Evaluator& evaluator, const Values& args) {
    if (args.empty() || args.size() > 4)
        throw std::runtime_error("copy expects 1 to 4 arguments");
    Value src = args[0];
    if (args.size() == 1) {
        if (!src.is_object()) return src;
        switch (src.as_object()->type()) {
            case ObjectType::Pair: {
                std::vector<Value> items;
                Value rest = src;
                while (rest.is_object() &&
                       rest.as_object()->type() == ObjectType::Pair) {
                    auto* pair = rest.as_object<PairObject>();
                    items.push_back(pair->car);
                    rest = pair->cdr;
                }
                Value result = rest;
                for (auto it = items.rbegin(); it != items.rend(); ++it)
                    result = evaluator.pair(*it, result);
                return result;
            }
            case ObjectType::Vector:
                return evaluator.vector(src.as_object<VectorObject>()->values);
            case ObjectType::String:
                return evaluator.string(src.as_object<StringObject>()->value);
            default:
                return src;
        }
    }

    auto length_of = [](Value sequence) -> std::int64_t {
        if (!sequence.is_object()) return -1;
        switch (sequence.as_object()->type()) {
            case ObjectType::String:
                return static_cast<std::int64_t>(
                    utf8_length(sequence.as_object<StringObject>()->value));
            case ObjectType::Vector:
                return static_cast<std::int64_t>(
                    sequence.as_object<VectorObject>()->values.size());
            case ObjectType::Pair: {
                std::int64_t count = 0;
                Value rest = sequence;
                while (rest.is_object() &&
                       rest.as_object()->type() == ObjectType::Pair) {
                    ++count;
                    rest = rest.as_object<PairObject>()->cdr;
                }
                return rest.is_null() ? count : -1;
            }
            default:
                return -1;
        }
    };
    Value dest = args[1];
    const std::int64_t source_length = length_of(src);
    const std::int64_t dest_length = length_of(dest);
    if (source_length < 0)
        throw std::runtime_error("copy source must be a sequence");
    if (dest_length < 0)
        throw std::runtime_error("copy target must be a sequence");
    const std::int64_t start = args.size() >= 3 ? args[2].as_integer() : 0;
    const std::int64_t end =
        args.size() >= 4 ? args[3].as_integer() : source_length;
    if (start < 0 || end < start || end > source_length)
        throw std::runtime_error("copy range out of bounds");
    if (end - start > dest_length)
        throw std::runtime_error("copy target too small");
    const std::int64_t count = end - start;

    const ObjectType source_type = src.as_object()->type();
    const ObjectType dest_type = dest.as_object()->type();

    if (source_type == ObjectType::String && dest_type == ObjectType::String) {
        const auto& source = src.as_object<StringObject>()->value;
        auto& target = dest.as_object<StringObject>()->value;
        const auto from = utf8_byte_offset(source, start);
        const auto until = utf8_byte_offset(source, end);
        const auto copied = source.substr(from, until - from);
        target.replace(0, utf8_byte_offset(target, count), copied);
        return dest;
    }
    if (source_type == ObjectType::Vector && dest_type == ObjectType::Vector) {
        auto& source = src.as_object<VectorObject>()->values;
        auto& target = dest.as_object<VectorObject>()->values;
        for (std::int64_t i = 0; i < count; ++i)
            target[static_cast<std::size_t>(i)] =
                source[static_cast<std::size_t>(start + i)];
        return dest;
    }
    if (source_type == ObjectType::String && dest_type == ObjectType::Vector) {
        const std::string& text = src.as_object<StringObject>()->value;
        auto& target = dest.as_object<VectorObject>()->values;
        auto position = utf8_byte_offset(text, start);
        for (std::int64_t i = 0; i < count; ++i) {
            std::size_t width = 0;
            target[static_cast<std::size_t>(i)] =
                evaluator.character(utf8_character_at(text, position, width));
            position += width;
        }
        return dest;
    }
    if (source_type == ObjectType::Vector && dest_type == ObjectType::String) {
        const auto& source = src.as_object<VectorObject>()->values;
        std::string& text = dest.as_object<StringObject>()->value;
        text.clear();
        for (std::int64_t i = 0; i < count; ++i) {
            Value element = source[static_cast<std::size_t>(start + i)];
            if (element.is_object() &&
                element.as_object()->type() == ObjectType::Character)
                text += utf8_encode_char(element.as_object<CharacterObject>()->value);
            else if (element.is_integer())
                text += utf8_encode_char(static_cast<char32_t>(element.as_integer()));
            else
                throw std::runtime_error(
                    "copy: target string cannot hold this element");
        }
        return dest;
    }
    if (source_type == ObjectType::Pair && dest_type == ObjectType::Pair) {
        Value source_rest = src;
        for (std::int64_t i = 0; i < start; ++i)
            source_rest = source_rest.as_object<PairObject>()->cdr;
        Value target_rest = dest;
        for (std::int64_t i = 0; i < count; ++i) {
            if (!target_rest.is_object() ||
                target_rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("copy target too small");
            target_rest.as_object<PairObject>()->car =
                source_rest.as_object<PairObject>()->car;
            source_rest = source_rest.as_object<PairObject>()->cdr;
            target_rest = target_rest.as_object<PairObject>()->cdr;
        }
        return dest;
    }
    throw std::runtime_error("copy: unsupported source/target combination");
}

} // namespace

bool equal(Value left, Value right) {
    return equal_values_impl(left, right);
}

bool equivalent(Value left, Value right) {
    if (!is_number(left) || !is_number(right)) return same(left, right);
    const Number a = number_value(left), b = number_value(right);
    if (a.is_exact() != b.is_exact() || !number_equal(a, b)) return false;
    // Component exactness is observable through real-part and imag-part.
    auto same_component = [](const RealNumber& x, const RealNumber& y) {
        return x.inexact == y.inexact &&
               (!x.inexact || !x.is_zero() || !y.is_zero() ||
                std::signbit(x.inexact_value) == std::signbit(y.inexact_value));
    };
    return same_component(a.real, b.real) && same_component(a.imag, b.imag);
}

bool is_proper_list(Value value) {
    auto pair = [](Value v) {
        return v.is_object() && v.as_object()->type() == ObjectType::Pair;
    };
    Value slow = value, fast = value;
    while (pair(fast)) {
        fast = fast.as_object<PairObject>()->cdr;
        if (!pair(fast)) return fast.is_null();
        fast = fast.as_object<PairObject>()->cdr;
        slow = slow.as_object<PairObject>()->cdr;
        if (fast == slow) return false;
    }
    return fast.is_null();
}

Value current_input_port_value() { return g_current_ports.input; }
Value current_output_port_value() { return g_current_ports.output; }
void set_current_input_port_value(Value port) { g_current_ports.input = port; }
void set_current_output_port_value(Value port) { g_current_ports.output = port; }

void install_runtime_primitives(Evaluator& evaluator) {
    install_random_primitives(evaluator);
    // Platform capability adapters are installed as a separate layer.
    install_platform_primitives(evaluator);

    // *load-path* is the reader/loader's real variable (install.scm
    // registers it as a set!-able toplevel): the test harness conses
    // fixture directories onto it, so initialize it from the search dirs
    // the driver configured (-I/-A land in the env var before this runs).
    {
        std::vector<Value> dirs;
        dirs.push_back(evaluator.string("goldfish"));
        if (const char* search_path =
                std::getenv("GOLDFISH_NATIVE_LOAD_PATH")) {
            std::stringstream paths(search_path);
            std::string directory;
            while (std::getline(paths, directory, ':'))
                if (!directory.empty())
                    dirs.push_back(evaluator.string(directory));
        }
        evaluator.global_environment()->define(
            evaluator.symbol("*load-path*"), evaluator.list(dirs));
    }

    // Ports and textual output are runtime objects; formatting policy stays
    // in Scheme libraries.  The current ports live in a slot so
    // with-input-from-file / with-output-to-file can rebind them.
    auto stdout_stream = std::shared_ptr<std::ostream>(&std::cout,
                                                       [](std::ostream*) {});
    auto stderr_stream = std::shared_ptr<std::ostream>(&std::cerr,
                                                       [](std::ostream*) {});
    g_current_ports.output = Value::object(
        evaluator.heap().make<OutputPortObject>(stdout_stream));
    g_current_ports.error_port = Value::object(
        evaluator.heap().make<OutputPortObject>(stderr_stream));
    auto write_text = [&evaluator](const Values& args, const char* name) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error(std::string(name) +
                                     " expects one or two arguments");
        Value port_value = args.size() == 2 ? args[1] : g_current_ports.output;
        *output_port(port_value, name).stream
            << format_value(evaluator, args[0],
                            std::string(name) != "display", 0);
        return Values{Value::unspecified()};
    };
    install(evaluator, "display",
            [write_text](const Values& args) {
                return write_text(args, "display");
            });
    install(evaluator, "write",
            [write_text](const Values& args) {
                return write_text(args, "write");
            });
    install(evaluator, "write-shared",
            [write_text](const Values& args) {
                return write_text(args, "write-shared");
            });
    install(evaluator, "write-simple",
            [write_text](const Values& args) {
                return write_text(args, "write-simple");
            });
    install(evaluator, "g-write-atom", [&evaluator](const Values& args) {
        require_arity(args, 3, "g-write-atom");
        if (args[0].is_object() &&
            (args[0].as_object()->type() == ObjectType::Pair ||
             args[0].as_object()->type() == ObjectType::Vector))
            throw std::runtime_error("g-write-atom expects an atomic value");
        *output_port(args[1], "g-write-atom").stream
            << format_value(evaluator, args[0], !args[2].as_boolean(), 0);
        return Values{Value::unspecified()};
    });
    install(evaluator, "newline", [](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("newline expects zero or one arguments");
        *output_port(args.size() == 1 ? args[0] : g_current_ports.output,
                     "newline").stream << '\n';
        return Values{Value::unspecified()};
    });
    install(evaluator, "write-char", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("write-char expects one or two arguments");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Character)
            raise_keyed(evaluator, "wrong-type-arg",
                        "write-char expects a character");
        Value port = args.size() == 2 ? args[1] : g_current_ports.output;
        *output_port(port, "write-char").stream
            << utf8_encode_char(evaluator.character_value(args[0]));
        return Values{Value::unspecified()};
    });
    install(evaluator, "write-string", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 4)
            throw std::runtime_error("write-string expects one to four arguments");
        const std::string& value = evaluator.string_value(args[0]);
        Value port = args.size() == 2 ? args[1] : g_current_ports.output;
        if (args.size() >= 3) port = args[1];
        std::int64_t start = 0;
        auto end = static_cast<std::int64_t>(utf8_length(value));
        if (args.size() >= 3) {
            if (!args[2].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "write-string start must be an integer");
            start = args[2].as_integer();
        }
        if (args.size() == 4) {
            if (!args[3].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "write-string end must be an integer");
            end = args[3].as_integer();
        }
        if (start < 0 || end < start ||
            static_cast<std::size_t>(end) > utf8_length(value))
            throw std::runtime_error(
                "out-of-range: write-string index out of bounds");
        const auto byte_start = utf8_byte_offset(value, start);
        const auto byte_end = utf8_byte_offset(value, end);
        *output_port(port, "write-string").stream << value.substr(
            byte_start, byte_end - byte_start);
        return Values{Value::unspecified()};
    });
    install(evaluator, "native-error-object?", [](const Values& args) {
        require_arity(args, 1, "native-error-object?");
        return Values{Value::boolean(
            args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::ErrorObject)};
    });
    install(evaluator, "native-error-object-message", [&evaluator](const Values& args) {
        require_arity(args, 1, "native-error-object-message");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::ErrorObject)
            throw std::runtime_error(
                "native-error-object-message expects an error object");
        return Values{evaluator.string(
            args[0].as_object<ErrorObject>()->message)};
    });
    install(evaluator, "%native-error-object-irritants", [&evaluator](const Values& args) {
        require_arity(args, 1, "error-object-irritants");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::ErrorObject)
            throw std::runtime_error("error-object-irritants expects an error object");
        return Values{evaluator.list(args[0].as_object<ErrorObject>()->irritants)};
    });
    // Integer atoms used by the kernel and by the bootstrap libraries.
    install(evaluator, "lognot", [&evaluator](const Values& args) {
        require_arity(args, 1, "lognot");
        const BigInteger value = exact_integer(args[0], "lognot");
        return Values{evaluator.number(Number::exact(-value - BigInteger(1)))};
    });
    for (const char* name : {"logand", "logior", "logxor"}) {
        install(evaluator, name, [name, &evaluator](const Values& args) {
            const bool is_and = std::strcmp(name, "logand") == 0;
            const bool is_or = std::strcmp(name, "logior") == 0;
            boost::multiprecision::cpp_int result = is_and ? -1 : 0;
            // Unchecked cpp_int bitwise operations use infinite two's complement.
            for (Value argument : args) {
                const BigInteger value = exact_integer(argument, name);
                if (is_and) result &= value.native();
                else if (is_or) result |= value.native();
                else result ^= value.native();
            }
            return Values{evaluator.number(Number::exact(BigInteger(std::move(result))))};
        });
    }
    install(evaluator, "ash", [&evaluator](const Values& args) {
        require_arity(args, 2, "ash");
        const BigInteger value = exact_integer(args[0], "ash");
        const BigInteger shift = exact_integer(args[1], "ash");
        if (shift.is_zero() || value.is_zero()) return Values{args[0]};
        const bool negative = value.negative();
        boost::multiprecision::cpp_int result;
        if (shift.negative()) {
            // Shift the complement so negative division rounds towards minus infinity.
            result = value.native();
            if (negative) result = -result - 1;
            const std::uint64_t width = result == 0 ? 0 :
                static_cast<std::uint64_t>(boost::multiprecision::msb(result)) + 1;
            const BigInteger amount = -shift;
            if (amount >= BigInteger(static_cast<std::int64_t>(width)))
                return Values{Value::integer(negative ? -1 : 0)};
            result >>= static_cast<unsigned>(amount.to_int64());
            if (negative) result = -result - 1;
        } else {
            result = value.native();
            if (negative) result = -result;
            const std::uint64_t width =
                static_cast<std::uint64_t>(boost::multiprecision::msb(result)) + 1;
            const std::uint64_t limit = std::numeric_limits<unsigned>::max();
            if (width > limit || !shift.fits_int64() ||
                shift > BigInteger(static_cast<std::int64_t>(limit - width)))
                throw std::runtime_error("ash: result exceeds the native bit-index range");
            result <<= static_cast<unsigned>(shift.to_int64());
            if (negative) result = -result;
        }
        return Values{evaluator.number(Number::exact(BigInteger(std::move(result))))};
    });
    install(evaluator, "bit-count", [](const Values& args) {
        require_arity(args, 1, "bit-count");
        const BigInteger value = exact_integer(args[0], "bit-count");
        boost::multiprecision::cpp_int magnitude = value.native();
        if (value.negative()) magnitude = -magnitude - 1;
        std::vector<std::uint64_t> chunks;
        boost::multiprecision::export_bits(magnitude, std::back_inserter(chunks), 64, false);
        std::uint64_t count = 0;
        for (std::uint64_t chunk : chunks) count += population_count(chunk);
        return Values{Value::integer(static_cast<std::int64_t>(count))};
    });
    install(evaluator, "integer-length", [](const Values& args) {
        require_arity(args, 1, "integer-length");
        const BigInteger value = exact_integer(args[0], "integer-length");
        boost::multiprecision::cpp_int magnitude = value.native();
        if (value.negative()) magnitude = -magnitude - 1;
        const std::int64_t length = magnitude == 0 ? 0 :
            static_cast<std::int64_t>(boost::multiprecision::msb(magnitude)) + 1;
        return Values{Value::integer(length)};
    });
    install(evaluator, "abs", [&evaluator](const Values& args) {
        require_arity(args, 1, "abs");
        if (!is_number(args[0])) throw std::runtime_error("abs expects a number");
        return Values{evaluator.number(number_abs(number_value(args[0])))};
    });
    for (const char* name : {"min", "max"}) {
        install(evaluator, name, [name, &evaluator](const Values& args) {
            if (args.empty())
                throw std::runtime_error(std::string(name) + " expects an argument");
            bool has_nan = false;
            for (const Value& arg : args) {
                if (!is_number(arg) || !number_value(arg).is_real())
                    throw std::runtime_error(std::string(name) +
                                             " expects real numbers");
                const RealNumber& real = number_value(arg).real;
                has_nan = has_nan ||
                    (real.inexact && std::isnan(real.inexact_value));
            }
            if (has_nan)
                return Values{evaluator.number(Number::inexact(
                    std::numeric_limits<double>::quiet_NaN()))};
            Value result = args[0];
            bool any_inexact = false;
            for (Value arg : args)
                any_inexact = any_inexact || !number_value(arg).is_exact();
            for (std::size_t i = 1; i < args.size(); ++i) {
                int order = number_compare(number_value(args[i]), number_value(result));
                if ((std::string(name) == "min" && order < 0) ||
                    (std::string(name) == "max" && order > 0))
                    result = args[i];
            }
            if (any_inexact) {
                Number selected = number_value(result);
                auto inexact = [](const RealNumber& part) {
                    return RealNumber::inexact_real(part.to_double());
                };
                result = evaluator.number(Number::complex(
                    inexact(selected.real), inexact(selected.imag)));
            }
            return Values{result};
        });
    }
    install(evaluator, "expt", [&evaluator](const Values& args) {
        require_arity(args, 2, "expt");
        if (!is_number(args[0]) || !is_number(args[1]))
            throw std::runtime_error("expt expects numbers");
        return Values{evaluator.number(number_expt(number_value(args[0]),
                                                  number_value(args[1])))};
    });
    // Explicit evaluation environments are the native interaction contract.
    // s7 surface: (gensym [prefix]) -> a fresh symbol per call.  The
    // high per-process base keeps generated names out of the space of
    // names user code is likely to define.
    install(evaluator, "gensym", [&evaluator](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("gensym expects zero or one argument");
        std::string prefix = "g";
        if (args.size() == 1) {
            if (!args[0].is_object() ||
                args[0].as_object()->type() != ObjectType::String)
                throw std::runtime_error("gensym prefix must be a string");
            prefix = args[0].as_object<StringObject>()->value;
        }
        static std::size_t counter = 1000000;
        return Values{
            evaluator.symbol(prefix + "-" + std::to_string(++counter))};
    });
    // Per-top-level-form boundary called from the Scheme loader's
    // expand-eval (liii/reader.scm): under the conservative collector a
    // collection here keeps the dirty heap to one form's worth while the
    // stack is shallow; exact tracing (host) would only lose time, so it
    // skips the collect.  Also hosts the GOLDFISH_DEBUG=progress
    // heartbeat for long loads.
    install(evaluator, "%form-boundary", [&evaluator](const Values& args) {
        (void)args;
        if (gc_mode() == GcMode::Conservative)
            evaluator.collect();
        if (debug_enabled("progress")) {
            static std::size_t form_count = 0;
            std::fprintf(stderr, "[progress] form %zu\n", ++form_count);
        }
        return Values{Value::unspecified()};
    });
    install(evaluator, "defined?", [&evaluator](const Values& args) {
        require_arity(args, 1, "defined?");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Symbol)
            return Values{Value::boolean(false)};
        try {
            (void)evaluator.global_environment()->lookup(args[0]);
            return Values{Value::boolean(true)};
        } catch (const std::runtime_error&) {
            return Values{Value::boolean(false)};
        }
    });
    install(evaluator, "symbol->value", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("symbol->value expects one or two arguments");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Symbol)
            throw std::runtime_error("symbol->value expects a symbol");
        if (args.size() == 2 &&
            (!args[1].is_object() ||
             args[1].as_object()->type() != ObjectType::EvalEnvironment))
            throw std::runtime_error("symbol->value expects an environment");
        try {
            if (args.size() == 2)
                return Values{args[1].as_object<EvalEnvironmentObject>()
                                  ->environment->lookup(args[0])};
            return Values{evaluator.global_environment()->lookup(args[0])};
        } catch (const std::runtime_error&) {
            throw std::runtime_error("unbound symbol");
        }
    });
    install(evaluator, "make-eval-environment",
            [&evaluator](const Values& args) {
                if (args.size() > 1)
                    throw std::runtime_error(
                        "make-eval-environment expects zero or one argument");
                if (args.empty())
                    return Values{evaluator.make_eval_environment()};
                if (!args[0].is_object() ||
                    args[0].as_object()->type() != ObjectType::EvalEnvironment)
                    throw std::runtime_error(
                        "make-eval-environment expects an eval environment parent");
                return Values{evaluator.make_eval_environment(
                    args[0].as_object<EvalEnvironmentObject>()->environment)};
            });
    install(evaluator, "eval-environment?", [](const Values& args) {
        require_arity(args, 1, "eval-environment?");
        return Values{Value::boolean(
            args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::EvalEnvironment)};
    });
    install(evaluator, "eval-environment-define!", [](const Values& args) {
        require_arity(args, 3, "eval-environment-define!");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::EvalEnvironment) {
            throw std::runtime_error(
                "eval-environment-define! expects an eval environment");
        }
        args[0].as_object<EvalEnvironmentObject>()->environment->define(
            args[1], args[2]);
        return Values{Value::unspecified()};
    });
    install(evaluator, "%eval-environment-link!", [](const Values& args) {
        require_arity(args, 4, "%eval-environment-link!");
        for (std::size_t index : {std::size_t(0), std::size_t(2)}) {
            if (!args[index].is_object() ||
                args[index].as_object()->type() != ObjectType::EvalEnvironment)
                throw std::runtime_error("environment link expects eval environments");
        }
        args[0].as_object<EvalEnvironmentObject>()->environment->link(
            args[1], *args[2].as_object<EvalEnvironmentObject>()->environment, args[3]);
        return Values{Value::unspecified()};
    });
    evaluator.define_machine_primitive("%current-eval-environment", PrimitiveObject::Kind::CurrentEnvironment);
    install(evaluator, "eval-environment-set!", [](const Values& args) {
        require_arity(args, 3, "eval-environment-set!");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error(
                "eval-environment-set! expects an eval environment");
        args[0].as_object<EvalEnvironmentObject>()->environment->set(args[1],
                                                                       args[2]);
        return Values{Value::unspecified()};
    });
    auto interaction_environment =
        std::make_shared<Value>(Value::unspecified());
    install(evaluator, "interaction-environment",
            [&evaluator, interaction_environment](const Values& args) {
                require_arity(args, 0, "interaction-environment");
                if (interaction_environment->is_unspecified())
                    *interaction_environment =
                        evaluator.make_eval_environment();
                return Values{*interaction_environment};
            });
    install(evaluator, "eval-environment-ref", [](const Values& args) {
        require_arity(args, 2, "eval-environment-ref");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error(
                "eval-environment-ref expects an eval environment");
        return Values{args[0].as_object<EvalEnvironmentObject>()
                          ->environment->lookup(args[1])};
    });
    evaluator.define_machine_primitive("eval", PrimitiveObject::Kind::Evaluate);
    // (scheme eval) uses this private binding so importing that library cannot
    // rebind the runtime evaluator into a recursive loop.
    evaluator.define_machine_primitive("%native-eval", PrimitiveObject::Kind::Evaluate);
    evaluator.define_callcc_primitive("call/cc");
    evaluator.define_callcc_primitive("call-with-current-continuation");
    evaluator.define_dynamic_wind_primitive("dynamic-wind");
    // Input/output ports and the tiny reader boundary.
    g_eof_singleton = Value::object(evaluator.heap().make<EofObject>());
    Value eof = g_eof_singleton;
    install(evaluator, "eof-object", [eof](const Values& args) {
        require_arity(args, 0, "eof-object");
        return Values{eof};
    });
    install(evaluator, "eof-object?", [](const Values& args) {
        require_arity(args, 1, "eof-object?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Eof)};
    });
    install(evaluator, "open-input-string", [&evaluator](const Values& args) {
        require_arity(args, 1, "open-input-string");
        return Values{Value::object(evaluator.heap().make<InputStringPortObject>(
            evaluator.string_value(args[0])))};
    });
    install(evaluator, "g-open-input-bytevector",
            [&evaluator](const Values& args) {
        require_arity(args, 1, "open-input-bytevector");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Bytevector)
            raise_keyed(evaluator, "type-error",
                        "open-input-bytevector: expected bytevector");
        return Values{Value::object(evaluator.heap().make<InputStringPortObject>(
            args[0].as_object<BytevectorObject>()->bytes))};
    });
    install(evaluator, "open-input-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "open-input-file");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::String)
            raise_keyed(evaluator, "type-error",
                        "open-input-file: expected string");
        const std::string path = evaluator.string_value(args[0]);
        std::ifstream input(path, std::ios::binary);
        if (!input)
            throw std::runtime_error("cannot open input file: " + path);
        std::string source((std::istreambuf_iterator<char>(input)),
                           std::istreambuf_iterator<char>());
        return Values{Value::object(
            evaluator.heap().make<InputStringPortObject>(std::move(source)))};
    });
    install(evaluator, "open-output-file", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("open-output-file expects one or two arguments");
        const std::string path = evaluator.string_value(args[0]);
        const bool append = args.size() == 2 && evaluator.string_value(args[1]) == "a";
        auto file = std::make_shared<std::ofstream>(
            path, std::ios::binary | (append ? std::ios::app : std::ios::trunc));
        if (!*file) throw std::runtime_error("cannot open output file: " + path);
        std::shared_ptr<std::ostream> stream = file;
        Value port = Value::object(
            evaluator.heap().make<OutputPortObject>(std::move(stream)));
        auto& output = *port.as_object<OutputPortObject>();
        output.file_path = path;
        output.file_backed = true;
        output.file_append = append;
        return Values{port};
    });
    install(evaluator, "open-output-string", [&evaluator](const Values& args) {
        require_arity(args, 0, "open-output-string");
        auto buffer = std::make_shared<std::string>();
        auto stream = std::make_shared<std::ostringstream>();
        return Values{Value::object(evaluator.heap().make<OutputPortObject>(
            std::move(stream), std::move(buffer)))};
    });
    install(evaluator, "output-port?", [](const Values& args) {
        require_arity(args, 1, "output-port?");
        return Values{Value::boolean(args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::OutputPort)};
    });
    install(evaluator, "current-output-port", [](const Values& args) {
        return port_parameter(g_current_ports.output, args,
                              ObjectType::OutputPort, "current-output-port");
    });
    install(evaluator, "close-output-port", [](const Values& args) {
        require_arity(args, 1, "close-output-port");
        auto& port = output_port(args[0], "close-output-port");
        port.stream->flush();
        port.closed = true;
        return Values{Value::unspecified()};
    });
    install(evaluator, "port-closed?", [](const Values& args) {
        require_arity(args, 1, "port-closed?");
        if (args[0].is_object()) {
            switch (args[0].as_object()->type()) {
            case ObjectType::InputPort:
                return Values{Value::boolean(
                    args[0].as_object<InputStringPortObject>()->closed)};
            case ObjectType::OutputPort:
                return Values{Value::boolean(
                    args[0].as_object<OutputPortObject>()->closed)};
            default:
                break;
            }
        }
        throw std::runtime_error("port-closed? expects a port");
    });
    install(evaluator, "flush-output-port", [](const Values& args) {
        require_arity(args, 1, "flush-output-port");
        output_port(args[0], "flush-output-port").stream->flush();
        return Values{Value::unspecified()};
    });
    install(evaluator, "input-port?", [](const Values& args) {
        require_arity(args, 1, "input-port?");
        return Values{Value::boolean(
            args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::InputPort)};
    });
    install(evaluator, "close-input-port", [](const Values& args) {
        require_arity(args, 1, "close-input-port");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("close-input-port expects an input port");
        args[0].as_object<InputStringPortObject>()->closed = true;
        return Values{Value::unspecified()};
    });
    install(evaluator, "file-exists?", [&evaluator](const Values& args) {
        require_arity(args, 1, "file-exists?");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::String)
            raise_keyed(evaluator, "type-error",
                        "file-exists?: expected string");
        const Value access = evaluator.global_environment()->lookup(
            evaluator.symbol("g_access"));
        auto has_access = [&](std::int64_t mode) {
            return evaluator.apply_values(access,
                {args[0], Value::integer(mode)})[0].as_boolean();
        };
        if (!has_access(0)) return Values{Value::boolean(false)};
        if (!has_access(1))
            raise_keyed(evaluator, "permission-error",
                        "file-exists?: no read permission");
        return Values{Value::boolean(true)};
    });
    install(evaluator, "load-find-module-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "load-find-module-file");
        const std::string requested = evaluator.string_value(args[0]);
        std::vector<std::string> candidates = {requested};
        // Search the Scheme-visible *load-path* first: the harness conses
        // fixture directories onto it, and library imports must find files
        // there.  The env var stays for dirs the dispatcher appends after
        // startup (its setenv is invisible to an already-initialized var).
        try {
            Value dirs = evaluator.global_environment()->lookup(
                evaluator.symbol("*load-path*"));
            for (Value rest = dirs;
                 rest.is_object() &&
                 rest.as_object()->type() == ObjectType::Pair;
                 rest = rest.as_object<PairObject>()->cdr) {
                Value dir = rest.as_object<PairObject>()->car;
                if (dir.is_object() &&
                    dir.as_object()->type() == ObjectType::String)
                    candidates.push_back(
                        (fs::path(evaluator.string_value(dir)) / requested)
                            .string());
            }
        } catch (const std::runtime_error&) {
        }
        if (const char* search_path = std::getenv("GOLDFISH_NATIVE_LOAD_PATH")) {
            std::stringstream paths(search_path);
            std::string directory;
            while (std::getline(paths, directory, ':'))
                if (!directory.empty())
                    candidates.push_back((fs::path(directory) / requested).string());
        }
        for (const std::string& path : candidates) {
            std::ifstream input(path, std::ios::binary);
            if (input) return Values{evaluator.string(path)};
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "read-forms", [&evaluator](const Values& args) {
        require_arity(args, 1, "read-forms");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("read-forms expects an input port");
        auto* port = args[0].as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("read-forms from closed input port");
        std::vector<Value> forms;
        TinyReader reader(evaluator, port->source, port->position);
        while (true) {
            std::optional<Value> form = reader.read();
            if (!form) break;
            forms.push_back(*form);
        }
        port->position = reader.position();
        return Values{evaluator.list(forms)};
    });
    install(evaluator, "port-position", [](const Values& args) {
        require_arity(args, 1, "port-position");
        auto& port = input_port(args[0], "port-position");
        if (port.closed)
            throw std::runtime_error("port-position on closed input port");
        return Values{Value::integer(
            static_cast<std::int64_t>(port.position))};
    });
    install(evaluator, "g-tiny-read", [&evaluator, eof](const Values& args) {
        require_arity(args, 1, "g-tiny-read");
        auto& port = input_port(args[0], "g-tiny-read");
        if (port.position == port.source.size()) return Values{eof};
        TinyReader reader(evaluator, port.source.substr(port.position));
        std::optional<Value> value = reader.read();
        port.position += reader.position();
        return Values{value ? *value : eof};
    });
    // Bootstrap-only source entry.  Once the expander is installed, source
    // forms must pass through expand-eval; evaluating raw forms here would
    // make ordinary derived forms (and, in particular, `and') look like
    // missing primitives.  Before expand-eval exists this remains the seed
    // evaluator used to bring the loader itself up.  Do not route this
    // through compile-file: compile-file itself is part of the expander
    // bootstrap and doing so creates a recursive loader dependency.
    install(evaluator, "load-source-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "load-source-file");
        const std::string requested = evaluator.string_value(args[0]);
        std::string path = requested;
        std::ifstream input(path, std::ios::binary);
        if (!input) {
            path = "goldfish/" + requested;
            input.clear();
            input.open(path, std::ios::binary);
        }
        if (!input) {
            if (const char* search_path = std::getenv("GOLDFISH_NATIVE_LOAD_PATH")) {
                std::stringstream paths(search_path);
                std::string directory;
                while (!input && std::getline(paths, directory, ':')) {
                    if (directory.empty()) continue;
                    path = (fs::path(directory) / requested).string();
                    input.clear();
                    input.open(path, std::ios::binary);
                }
            }
        }
        if (!input)
            throw std::runtime_error("load-source-file: cannot open " + requested);
        std::string source((std::istreambuf_iterator<char>(input)),
                           std::istreambuf_iterator<char>());
        const bool seed_file = requested == "core/gfo.scm" ||
                               path == "goldfish/core/gfo.scm";
        const bool prelude_file = requested == "liii/prelude.scm" ||
                                  path == "goldfish/liii/prelude.scm";
        auto lookup_bootstrap_binding = [&evaluator](const char* name) {
            try {
                return evaluator.global_environment()->lookup(
                    evaluator.symbol(name));
            } catch (const std::runtime_error&) {
                Value expander = evaluator.global_environment()->lookup(
                    evaluator.symbol("the-expander-library"));
                Value module_environment = evaluator.apply_values(
                    evaluator.global_environment()->lookup(
                        evaluator.symbol("module-eval-environment")),
                    {expander})[0];
                return module_environment.as_object<EvalEnvironmentObject>()
                    ->environment->lookup(evaluator.symbol(name));
            }
        };
        Value expand_eval;
        try {
            expand_eval = lookup_bootstrap_binding("expand-eval");
        } catch (const std::runtime_error&) {
            expand_eval = Value::unspecified();
        }
        try {
            std::vector<Value> datums;
            TinyReader reader(evaluator, std::move(source));
            while (std::optional<Value> form = reader.read())
                datums.push_back(*form);
            if (prelude_file && !expand_eval.is_unspecified()) {
                // Prelude transformers are intentionally installed into the
                // base library.  Process them one at a time so each macro is
                // visible to the next transformer body; the normal source
                // unit path uses a temporary library instead.
                Value compile_toplevel = lookup_bootstrap_binding(
                    "compile-toplevel");
                Value result = Value::unspecified();
                for (std::size_t index = 0; index < datums.size(); ++index) {
                    try {
                        Value lowered = evaluator.apply_values(
                            compile_toplevel, {datums[index]})[0];
                        result = evaluator.eval(lowered);
                    } catch (const std::exception& error) {
                        throw std::runtime_error(
                            "prelude form " + std::to_string(index) + ": " +
                            error.what());
                    }
                }
                return Values{result};
            }
            if (seed_file && !expand_eval.is_unspecified()) {
                // core/gfo.scm is the one source file that must establish the
                // cache layer itself.  Install it as a real exp-library,
                // linked explicitly to the base library, then publish its
                // value definitions into the root evaluator.  In particular,
                // do not evaluate one raw `define' at a time: that loses the
                // library's expansion context and makes recursive helpers
                // resolve through the wrong environment.
                Value make_exp_library =
                    lookup_bootstrap_binding("make-exp-library");
                Value seed_library = evaluator.apply_values(
                    make_exp_library,
                    {evaluator.list({evaluator.symbol("gfo-seed")})})[0];
                Value base_library = evaluator.global_environment()->lookup(
                    evaluator.symbol("the-base-library"));
                Value add_use = lookup_bootstrap_binding("exp-library-add-use!");
                evaluator.apply_values(add_use, {seed_library, base_library});

                Value wrap_expression =
                    lookup_bootstrap_binding("wrap-expression");
                Value set_library = lookup_bootstrap_binding("stx-set-library");
                Value initial_context =
                    lookup_bootstrap_binding("initial-context");
                Value expand_library_body =
                    lookup_bootstrap_binding("expand-library-body");
                Value lower = lookup_bootstrap_binding("lower");
                std::vector<Value> syntax_forms;
                for (Value datum : datums) {
                    Value syntax = evaluator.apply_values(
                        wrap_expression, {datum})[0];
                    syntax_forms.push_back(evaluator.apply_values(
                        set_library, {syntax, seed_library})[0]);
                }
                Value context = evaluator.apply_values(initial_context, {})[0];
                Values expanded;
                try {
                    expanded = evaluator.apply_values(
                        expand_library_body,
                        {evaluator.list(syntax_forms), seed_library, context});
                } catch (const std::exception& error) {
                    throw std::runtime_error(
                        std::string("gfo seed expansion: ") + error.what());
                }
                if (expanded.empty())
                    throw std::runtime_error("seed expansion returned no definitions");
                Value definitions = expanded[0];
                Value module_environment = evaluator.apply_values(
                    lookup_bootstrap_binding("module-eval-environment"),
                    {evaluator.global_environment()->lookup(
                        evaluator.symbol("the-expander-library"))})[0];
                Value binding_kind = lookup_bootstrap_binding("binding-kind");
                Value binding_value = lookup_bootstrap_binding("binding-value");
                Value toplevel_ref =
                    lookup_bootstrap_binding("toplevel-ref-gensym");
                Value bindings = evaluator.apply_values(
                    lookup_bootstrap_binding("exp-library-bindings"),
                    {seed_library})[0];
                EnvironmentPtr eval_environment =
                    module_environment.as_object<EvalEnvironmentObject>()
                        ->environment;
                Value result = Value::unspecified();
                std::size_t seed_definition_index = 0;
                for (Value definition : proper_list(definitions)) {
                    try {
                        result = evaluator.apply_values(lower, {definition})[0];
                        result = evaluator.eval(result, eval_environment);
                    } catch (const std::exception& error) {
                        throw std::runtime_error(
                            "seed definition " +
                            std::to_string(seed_definition_index) + ": " +
                            error.what());
                    }
                    ++seed_definition_index;
                }
                for (Value entry : proper_list(bindings)) {
                    if (!entry.is_object() ||
                        entry.as_object()->type() != ObjectType::Pair)
                        continue;
                    auto* pair = entry.as_object<PairObject>();
                    Value binding = pair->cdr;
                    if (!symbol_named(
                            evaluator.apply_values(binding_kind, {binding})[0],
                            "toplevel"))
                        continue;
                    Value reference = evaluator.apply_values(
                        binding_value, {binding})[0];
                    Value gensym = evaluator.apply_values(
                        toplevel_ref, {reference})[0];
                    evaluator.global_environment()->define(
                        pair->car, evaluator.eval(gensym, eval_environment));
                }
                return Values{result};
            }
            const bool program_source =
                requested == "goldfish/expander/build-combined.scm" ||
                path == "goldfish/expander/build-combined.scm";
            if (program_source && !expand_eval.is_unspecified()) {
                Value import_form = evaluator.list({
                    evaluator.symbol("import"),
                    evaluator.list({evaluator.symbol("except"),
                                    evaluator.list({evaluator.symbol("goldfish")}),
                                    evaluator.symbol("bytevector?"),
                                    evaluator.symbol("make-bytevector"),
                                    evaluator.symbol("bytevector"),
                                    evaluator.symbol("bytevector-length"),
                                    evaluator.symbol("bytevector-u8-ref"),
                                    evaluator.symbol("bytevector-u8-set!"),
                                    evaluator.symbol("bytevector-copy"),
                                    evaluator.symbol("bytevector-copy!"),
                                    evaluator.symbol("bytevector-append"),
                                    evaluator.symbol("bytevector->u8-list"),
                                    evaluator.symbol("u8-list->bytevector"),
                                    evaluator.symbol("utf8->string"),
                                    evaluator.symbol("string->utf8"),
                                    evaluator.symbol("bytevector-advance-utf8"),
                                    evaluator.symbol("utf8-string-length")})});
                evaluator.apply_values(expand_eval, {import_form});
                Value result = Value::unspecified();
                for (Value datum : datums)
                    result = evaluator.apply_values(expand_eval, {datum})[0];
                return Values{result};
            }
            if (!expand_eval.is_unspecified()) {
                // Bootstrap source files are programs rather than declared
                // libraries, but their definitions still need one shared
                // expansion unit so forward references resolve together.
                Value make_exp_library = lookup_bootstrap_binding(
                    "make-exp-library");
                Value base_library = evaluator.global_environment()->lookup(
                    evaluator.symbol("the-base-library"));
                const bool seed_source = requested == "liii/prelude.scm" ||
                    path == "goldfish/liii/prelude.scm" ||
                    requested == "expander/bootstrap-prelude.scm" ||
                    path == "goldfish/expander/bootstrap-prelude.scm";
                const bool internal_source = seed_source ||
                    requested == "expander/lib/install.scm" ||
                    path.find("goldfish/expander/lib/") == 0;
                Value source_library = seed_source
                    ? base_library
                    : internal_source
                        ? evaluator.apply_values(
                              make_exp_library,
                              {evaluator.list({evaluator.symbol("native-source")})})[0]
                        : evaluator.apply_values(
                              lookup_bootstrap_binding("make-program-library"), {})[0];
                if (!internal_source && !seed_source) {
                    // A loaded source file is a program body.  Its leading
                    // import declarations belong to the program library,
                    // not to the expression pass; remove them after
                    // applying the same Scheme import-set machinery used by
                    // the expander.
                    //
                    // The implementation library is ambient fallback only:
                    // register its use FIRST so the file's own imports sit
                    // in front of it and shadow it (uses resolve newest
                    // first, and exp-library-add-use! conses to the front).
                    evaluator.apply_values(
                        lookup_bootstrap_binding("exp-library-add-use!"),
                        {source_library, base_library});
                    Value import_symbol = evaluator.symbol("import");
                    Value import_into = evaluator.apply_values(
                        lookup_bootstrap_binding("module-ref"),
                        {evaluator.global_environment()->lookup(
                             evaluator.symbol("the-expander-library")),
                         evaluator.symbol("import-into-library!")})[0];
                    std::vector<Value> body_datums;
                    for (Value datum : datums) {
                        std::vector<Value> form;
                        if (datum.is_object() &&
                            datum.as_object()->type() == ObjectType::Pair)
                            form = proper_list(datum);
                        if (!form.empty() && form[0] == import_symbol) {
                            try {
                                evaluator.apply_values(import_into,
                                                       {source_library,
                                                        evaluator.list({
                                                            evaluator.list(
                                                                std::vector<Value>(
                                                                    form.begin() + 1,
                                                                    form.end()))})});
                            } catch (const RaisedValue& raised) {
                                throw std::runtime_error(
                                    std::string("source import: ") +
                                    raised_message(raised));
                            }
                        } else {
                            body_datums.push_back(datum);
                        }
                    }
                    datums = std::move(body_datums);
                }
                if (!seed_source)
                    evaluator.apply_values(
                        lookup_bootstrap_binding("exp-library-add-use!"),
                        {source_library, base_library});
                // Source bootstrap must not inherit a reader binding from an
                // older cached reader artifact.  Pin this one dependency at
                // the source unit boundary; ordinary programs still use the
                // Scheme reader through their normal imports.
                Value native_read_forms = evaluator.apply_values(
                    lookup_bootstrap_binding("make-primitive-binding"),
                    {evaluator.symbol("read-forms")})[0];
                evaluator.apply_values(
                    lookup_bootstrap_binding("exp-library-define!"),
                    {source_library, evaluator.symbol("read-forms"),
                     native_read_forms});
                Value wrap_expression = lookup_bootstrap_binding("wrap-expression");
                Value set_library = lookup_bootstrap_binding("stx-set-library");
                std::vector<Value> syntax_forms;
                for (Value datum : datums) {
                    Value syntax = evaluator.apply_values(
                        wrap_expression, {datum})[0];
                    syntax_forms.push_back(evaluator.apply_values(
                        set_library, {syntax, source_library})[0]);
                }
                Values expanded;
                try {
                    expanded = evaluator.apply_values(
                        lookup_bootstrap_binding("expand-library-body"),
                        {evaluator.list(syntax_forms), source_library,
                         evaluator.apply_values(
                             lookup_bootstrap_binding("initial-context"), {})[0]});
                } catch (const RaisedValue& raised) {
                    throw std::runtime_error(std::string("source expansion: ") +
                                             raised_message(raised));
                } catch (const std::exception& error) {
                    throw std::runtime_error(std::string("source expansion: ") +
                                             error.what());
                }
                Value definitions = expanded[0];
                Value module_environment = evaluator.apply_values(
                    lookup_bootstrap_binding("module-eval-environment"),
                    {evaluator.global_environment()->lookup(
                        evaluator.symbol("the-expander-library"))})[0];
                EnvironmentPtr eval_environment =
                    module_environment.as_object<EvalEnvironmentObject>()
                        ->environment;
                Value result = Value::unspecified();
                Value lower = lookup_bootstrap_binding("lower");
                std::size_t definition_index = 0;
                for (Value definition : proper_list(definitions)) {
                    try {
                        Value lowered_definition =
                            evaluator.apply_values(lower, {definition})[0];
                        result = evaluator.eval(lowered_definition, eval_environment);
                    } catch (const RaisedValue& raised) {
                        std::string detail = "raised a Scheme error";
                        if (raised.value().is_object() &&
                            raised.value().as_object()->type() == ObjectType::ErrorObject) {
                            const auto* error =
                                raised.value().as_object<ErrorObject>();
                            detail = error->message;
                            for (Value irritant : error->irritants) {
                                detail += " ";
                                detail += format_value(evaluator, irritant,
                                                       true, 0);
                            }
                        }
                        throw std::runtime_error(
                            "source definition " +
                            std::to_string(definition_index) + ": " + detail);
                    } catch (const std::exception& error) {
                        throw std::runtime_error(
                            "source definition " +
                            std::to_string(definition_index) + ": " +
                            error.what());
                    }
                    ++definition_index;
                }
                Value source_bindings = evaluator.apply_values(
                     lookup_bootstrap_binding("exp-library-bindings"),
                         {source_library})[0];
                for (Value entry : proper_list(source_bindings)) {
                    if (!entry.is_object() ||
                        entry.as_object()->type() != ObjectType::Pair)
                        continue;
                    Value name = entry.as_object<PairObject>()->car;
                    Value binding = entry.as_object<PairObject>()->cdr;
                    if (evaluator.apply_values(
                            lookup_bootstrap_binding("binding-kind"),
                            {binding})[0].as_object<SymbolObject>()->name !=
                        "toplevel")
                        continue;
                    Value reference = evaluator.apply_values(
                        lookup_bootstrap_binding("binding-value"), {binding})[0];
                    Value gensym = evaluator.apply_values(
                        lookup_bootstrap_binding("toplevel-ref-gensym"),
                        {reference})[0];
                    try {
                        evaluator.global_environment()->define(
                            name, evaluator.eval(gensym, eval_environment));
                    } catch (const std::runtime_error& error) {
                        throw std::runtime_error(
                            "source binding alias " +
                            name.as_object<SymbolObject>()->name + ": " +
                            error.what());
                    }
                }
                return Values{result};
            }
            Value result = Value::unspecified();
            for (std::size_t index = 0; index < datums.size(); ++index) {
                try {
                    result = evaluator.eval(datums[index]);
                } catch (const RaisedValue& raised) {
                    std::string detail = "raised a Scheme error";
                    if (raised.value().is_object() &&
                        raised.value().as_object()->type() == ObjectType::ErrorObject)
                        {
                            const auto* error =
                                raised.value().as_object<ErrorObject>();
                            detail = error->message;
                            for (Value irritant : error->irritants) {
                                detail += " ";
                                if (irritant.is_object() &&
                                    irritant.as_object()->type() == ObjectType::Symbol)
                                    detail += irritant.as_object<SymbolObject>()->name;
                                else if (irritant.is_object() &&
                                         irritant.as_object()->type() == ObjectType::String)
                                    detail += evaluator.string_value(irritant);
                                else if (irritant.is_integer())
                                    detail += std::to_string(irritant.as_integer());
                                else
                                    detail += "<value>";
                            }
                        }
                    throw std::runtime_error("source form " +
                                             std::to_string(index) + ": " +
                                             detail);
                }
            }
            return Values{result};
        } catch (const RaisedValue& raised) {
            throw std::runtime_error("load-source-file: evaluated " + path +
                                     ": " + raised_message(raised));
        } catch (const std::exception& error) {
            throw std::runtime_error("load-source-file: evaluated " + path +
                                     ": " + error.what());
        }
    });
    // Source bootstrap helpers.  They are deliberately primitive operations;
    // source policy and compilation remain in the Scheme expander.
    install(evaluator, "g-read-token", [&evaluator](const Values& args) {
        require_arity(args, 2, "g-read-token");
        auto& port = input_port(args[0], "g-read-token");
        char32_t first = evaluator.character_value(args[1]);
        std::string token;
        if (first <= 0x7f) token.push_back(static_cast<char>(first));
        else append_utf8(token, static_cast<unsigned>(first));
        while (port.position < port.source.size() &&
               !source_delimiter(static_cast<unsigned char>(port.source[port.position])))
            token.push_back(port.source[port.position++]);
        return Values{evaluator.string(token)};
    });
    install(evaluator, "g-read-string", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("g-read-string expects one or two arguments");
        auto& port = input_port(args[0], "g-read-string");
        char32_t delimiter = args.size() == 2 ? evaluator.character_value(args[1]) : U'"';
        std::string result;
        while (port.position < port.source.size()) {
            unsigned char c = static_cast<unsigned char>(port.source[port.position++]);
            if (c == delimiter) return Values{evaluator.string(result)};
            if (c != '\\') { result.push_back(static_cast<char>(c)); continue; }
            if (port.position == port.source.size())
                throw std::runtime_error("g-read-string: unterminated escape");
            unsigned char escaped = static_cast<unsigned char>(port.source[port.position++]);
            switch (escaped) {
            case 'a': result.push_back('\a'); break;
            case 'b': result.push_back('\b'); break;
            case 't': result.push_back('\t'); break;
            case 'n': result.push_back('\n'); break;
            case 'r': result.push_back('\r'); break;
            case 'f': result.push_back('\f'); break;
            case 'v': result.push_back('\v'); break;
            case '0': result.push_back('\0'); break;
            case 'e': result.push_back('\x1b'); break;
            case '\\': result.push_back('\\'); break;
            case '"': result.push_back('"'); break;
            case '|': result.push_back('|'); break;
            case 'x': {
                unsigned value = 0;
                std::size_t digits = 0;
                while (port.position < port.source.size()) {
                    unsigned char h = static_cast<unsigned char>(port.source[port.position]);
                    unsigned digit = h >= '0' && h <= '9' ? h - '0' :
                                     h >= 'a' && h <= 'f' ? h - 'a' + 10 :
                                     h >= 'A' && h <= 'F' ? h - 'A' + 10 : 16;
                    if (digit >= 16) break;
                    value = value * 16 + digit;
                    ++digits;
                    ++port.position;
                }
                if (digits == 0 || port.position == port.source.size() ||
                    port.source[port.position++] != ';')
                    throw std::runtime_error("g-read-string: invalid hex escape");
                append_utf8(result, value);
                break;
            }
            default:
                throw std::runtime_error("g-read-string: invalid escape");
            }
        }
        throw std::runtime_error("g-read-string: unterminated string");
    });
    install(evaluator, "g-undefined", [](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("g-undefined expects zero or one arguments");
        return Values{Value::unspecified()};
    });
    install(evaluator, "peek-char", [&evaluator, eof](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("peek-char expects zero or one arguments");
        Value port_value = args.empty() ? current_input_port(evaluator) : args[0];
        if (!port_value.is_object() ||
            port_value.as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("peek-char expects an input port");
        auto* port = port_value.as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("peek-char from closed input port");
        if (port->position == port->source.size()) return Values{eof};
        std::size_t width = 0;
        return Values{evaluator.character(
            utf8_character_at(port->source, port->position, width))};
    });
    install(evaluator, "read-char", [&evaluator, eof](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("read-char expects zero or one arguments");
        Value port_value = args.empty() ? current_input_port(evaluator) : args[0];
        if (!port_value.is_object() ||
            port_value.as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("read-char expects an input port");
        auto* port = port_value.as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("read-char from closed input port");
        if (port->position == port->source.size()) return Values{eof};
        std::size_t width = 0;
        const auto codepoint = utf8_character_at(port->source, port->position, width);
        port->position += width;
        return Values{evaluator.character(codepoint)};
    });
    for (const char* name : {"read-u8", "peek-u8"}) {
        install(evaluator, name, [name, &evaluator, eof](const Values& args) {
            if (args.size() > 1)
                throw std::runtime_error(std::string(name) + " expects zero or one argument");
            auto& port = input_port(args.empty() ? current_input_port(evaluator) : args[0], name);
            if (port.position == port.source.size()) return Values{eof};
            const auto byte = static_cast<unsigned char>(port.source[port.position]);
            if (std::string(name) == "read-u8") ++port.position;
            return Values{Value::integer(byte)};
        });
    }
    install(evaluator, "write-u8", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("write-u8 expects one or two arguments");
        if (!args[0].is_integer())
            raise_keyed(evaluator, "wrong-type-arg", "write-u8 expects an integer byte");
        const auto byte = args[0].as_integer();
        if (byte < 0 || byte > 255)
            raise_keyed(evaluator, "out-of-range", "write-u8 byte out of range");
        output_port(args.size() == 2 ? args[1] : g_current_ports.output, "write-u8")
            .stream->put(static_cast<char>(byte));
        return Values{Value::unspecified()};
    });
    install(evaluator, "g-delimiter?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g-delimiter?");
        char32_t character = evaluator.character_value(args[0]);
        return Values{Value::boolean(character == U'\0' ||
                                     (character <= 0x7f && std::isspace(static_cast<unsigned char>(character))) ||
                                     character == U'(' || character == U')' ||
                                     character == U'[' || character == U']' ||
                                     character == U'"' || character == U';')};
    });
    install(evaluator, "g-valid-identifier?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g-valid-identifier?");
        const std::string& identifier = evaluator.string_value(args[0]);
        const bool valid = !identifier.empty() &&
            std::none_of(identifier.begin(), identifier.end(), [](unsigned char c) {
                if (std::isspace(c)) return true;
                switch (c) {
                case '(':
                case ')':
                case '[':
                case ']':
                case '\'':
                case '`':
                case ',':
                case ';':
                case '"':
                case '|':
                case '\\':
                    return true;
                default:
                    return false;
                }
            });
        return Values{Value::boolean(valid)};
    });
    install(evaluator, "g-enabled?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g-enabled?");
        return Values{Value::boolean(false)};
    });
    evaluator.define_machine_primitive(
        "call-with-input-file", PrimitiveObject::Kind::CallWithInputFile);
    evaluator.define_machine_primitive(
        "call-with-output-file", PrimitiveObject::Kind::CallWithOutputFile);
    install(evaluator, "get-output-string", [&evaluator](const Values& args) {
        require_arity(args, 1, "get-output-string");
        auto& port = output_port(args[0], "get-output-string");
        auto stream = std::dynamic_pointer_cast<std::ostringstream>(port.stream);
        if (!stream || !port.buffer)
            throw std::runtime_error("get-output-string expects a string port");
        return Values{evaluator.string(stream->str())};
    });
    install(evaluator, "get-output-bytevector", [&evaluator](const Values& args) {
        require_arity(args, 1, "get-output-bytevector");
        auto& port = output_port(args[0], "get-output-bytevector");
        auto stream = std::dynamic_pointer_cast<std::ostringstream>(port.stream);
        if (!stream || !port.buffer)
            throw std::runtime_error("get-output-bytevector expects a buffered port");
        return Values{Value::object(evaluator.heap().make<BytevectorObject>(stream->str()))};
    });
    // String-port conveniences: s7 ships these as builtins, so the kernel's
    // primitive-variables list turns every reference into a bare name that
    // must resolve in the global environment.
    evaluator.define_machine_primitive(
        "with-output-to-string", PrimitiveObject::Kind::WithOutputToString);
    evaluator.define_machine_primitive(
        "call-with-output-string", PrimitiveObject::Kind::CallWithOutputString);
    evaluator.define_machine_primitive(
        "with-input-from-string", PrimitiveObject::Kind::WithInputFromString);
    evaluator.define_machine_primitive(
        "call-with-input-string", PrimitiveObject::Kind::CallWithInputString);
    install(evaluator, "delete-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "delete-file");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::String)
            raise_keyed(evaluator, "type-error",
                        "delete-file: expected string");
        const Value file_exists = evaluator.global_environment()->lookup(
            evaluator.symbol("file-exists?"));
        if (!evaluator.apply_values(file_exists, {args[0]})[0].as_boolean())
            raise_keyed(evaluator, "read-error",
                        "delete-file: no such file");
        std::error_code error;
        const bool removed =
            fs::remove(evaluator.string_value(args[0]), error);
        if (!removed || error)
            // Dialect parity: the host raises read-error for a missing
            // target and returns #t on success (the tests pin both).
            // libstdc++ does not always set ec for ENOENT, so the return
            // value decides.
            raise_keyed(evaluator, "read-error",
                        "delete-file: no such file");
        return Values{Value::boolean(true)};
    });
    install(evaluator, "read", [&evaluator, eof](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("read expects zero or one arguments");
        // Zero arguments reads from the current input port (R7RS).
        Value port_value =
            args.empty() ? current_input_port(evaluator) : args[0];
        if (!port_value.is_object() ||
            port_value.as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("read expects an input port");
        auto* port = port_value.as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("read from closed input port");
        if (port->position == port->source.size())
            return Values{eof};
        TinyReader reader(evaluator,
                          port->source.substr(port->position));
        std::optional<Value> value = reader.read();
        port->position += reader.position();
        return Values{value ? *value : eof};
    });
    // Core object predicates and pair/vector operations.
    install(evaluator, "boolean?", [](const Values& args) {
        require_arity(args, 1, "boolean?");
        return Values{Value::boolean(args[0].is_boolean())};
    });
    install(evaluator, "integer?", [](const Values& args) {
        require_arity(args, 1, "integer?");
        return Values{Value::boolean(is_number(args[0]) && number_value(args[0]).is_integer())};
    });
    install(evaluator, "number?", [](const Values& args) {
        require_arity(args, 1, "number?");
        return Values{Value::boolean(is_number(args[0]))};
    });
    install(evaluator, "real?", [](const Values& args) {
        require_arity(args, 1, "real?");
        return Values{Value::boolean(is_number(args[0]) && number_value(args[0]).is_real())};
    });
    install(evaluator, "exact-integer?", [](const Values& args) {
        require_arity(args, 1, "exact-integer?");
        return Values{Value::boolean(is_number(args[0]) && number_value(args[0]).is_integer() && number_value(args[0]).is_exact())};
    });
    for (const char* name : {"odd?", "even?"}) {
        install(evaluator, name, [name](const Values& args) {
            require_arity(args, 1, name);
            BigInteger integer;
            if (!is_number(args[0]) || !number_value(args[0]).is_integer() ||
                !number_value(args[0]).is_exact())
                throw std::runtime_error(std::string(name) +
                                         " expects an integer");
            integer = number_value(args[0]).real.numerator;
            bool odd = (integer.native() % 2) != 0;
            return Values{Value::boolean(name[0] == 'o' ? odd : !odd)};
        });
    }
    install(evaluator, "rational?", [](const Values& args) {
        require_arity(args, 1, "rational?");
        if (!is_number(args[0])) return Values{Value::boolean(false)};
        Number n = number_value(args[0]);
        const bool finite = !n.real.inexact || std::isfinite(n.real.inexact_value);
        return Values{Value::boolean(n.is_real() && finite)};
    });
    install(evaluator, "complex?", [](const Values& args) {
        require_arity(args, 1, "complex?");
        return Values{Value::boolean(is_number(args[0]))};
    });
    install(evaluator, "exact?", [](const Values& args) {
        require_arity(args, 1, "exact?");
        if (!is_number(args[0]))
            throw std::runtime_error("exact? expects a number");
        return Values{Value::boolean(number_value(args[0]).is_exact())};
    });
    install(evaluator, "inexact?", [](const Values& args) {
        require_arity(args, 1, "inexact?");
        if (!is_number(args[0]))
            throw std::runtime_error("inexact? expects a number");
        return Values{Value::boolean(!number_value(args[0]).is_exact())};
    });
    install(evaluator, "finite?", [](const Values& args) {
        require_arity(args, 1, "finite?");
        if (!is_number(args[0])) return Values{Value::boolean(false)};
        Number n = number_value(args[0]);
        return Values{Value::boolean((!n.real.inexact || std::isfinite(n.real.inexact_value)) &&
            (!n.has_imaginary_part || !n.imag.inexact || std::isfinite(n.imag.inexact_value)))};
    });
    install(evaluator, "infinite?", [](const Values& args) {
        require_arity(args, 1, "infinite?");
        if (!is_number(args[0])) return Values{Value::boolean(false)};
        { Number n = number_value(args[0]); return Values{Value::boolean(
            (n.real.inexact && std::isinf(n.real.inexact_value)) ||
            (n.has_imaginary_part && n.imag.inexact && std::isinf(n.imag.inexact_value)))}; }
    });
    install(evaluator, "nan?", [](const Values& args) {
        require_arity(args, 1, "nan?");
        if (!is_number(args[0])) return Values{Value::boolean(false)};
        { Number n = number_value(args[0]); return Values{Value::boolean(
            (n.real.inexact && std::isnan(n.real.inexact_value)) ||
            (n.has_imaginary_part && n.imag.inexact && std::isnan(n.imag.inexact_value)))}; }
    });
    auto exact_conversion = [&evaluator](const Values& args) -> Values {
        if (!is_number(args[0])) throw std::runtime_error("exact expects a number");
        Number n = number_value(args[0]);
        if (n.is_exact()) return Values{args[0]};
        auto convert = [](const RealNumber& r) {
            if (!r.inexact) return r;
            return exact_from_double(r.inexact_value);
        };
        return Values{evaluator.number(Number::complex(convert(n.real), convert(n.imag)))};
    };
    for (const char* name : {"exact", "inexact->exact"})
        install(evaluator, name, [name, exact_conversion](const Values& args) {
            require_arity(args, 1, name);
            return exact_conversion(args);
        });
    auto inexact_conversion = [&evaluator](const Values& args) -> Values {
        if (!is_number(args[0])) throw std::runtime_error("inexact expects a number");
        Number n = number_value(args[0]);
        if (!n.is_exact()) return Values{args[0]};
        if (!n.has_imaginary_part)
            return Values{evaluator.number(Number::inexact(n.real.to_double()))};
        auto convert = [](const RealNumber& r) {
            return RealNumber::inexact_real(r.to_double());
        };
        return Values{evaluator.number(Number::complex(convert(n.real), convert(n.imag)))};
    };
    for (const char* name : {"inexact", "exact->inexact"})
        install(evaluator, name, [name, inexact_conversion](const Values& args) {
            require_arity(args, 1, name);
            return inexact_conversion(args);
        });
    install(evaluator, "numerator", [&evaluator](const Values& args) {
        require_arity(args, 1, "numerator");
        if (!is_number(args[0]) || !number_value(args[0]).is_real())
            throw std::runtime_error("numerator expects a real number");
        RealNumber n = number_value(args[0]).real;
        if (n.inexact) n = exact_from_double(n.inexact_value);
        return Values{evaluator.number(Number::exact(n.numerator))};
    });
    install(evaluator, "denominator", [&evaluator](const Values& args) {
        require_arity(args, 1, "denominator");
        if (!is_number(args[0]) || !number_value(args[0]).is_real())
            throw std::runtime_error("denominator expects a real number");
        RealNumber n = number_value(args[0]).real;
        if (n.inexact) n = exact_from_double(n.inexact_value);
        return Values{evaluator.number(Number::exact(n.denominator))};
    });
    install(evaluator, "rationalize", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 2)
            throw std::runtime_error("rationalize expects one or two arguments");
        if (!is_number(args[0]) || !number_value(args[0]).is_real() ||
            (args.size() == 2 && (!is_number(args[1]) ||
                                  !number_value(args[1]).is_real())))
            throw std::runtime_error("rationalize expects a real number");
        auto exact_real = [](RealNumber value) {
            return value.inexact ? exact_from_double(value.inexact_value)
                                 : value;
        };
        RealNumber x = exact_real(number_value(args[0]).real);
        RealNumber tolerance = args.size() == 2
            ? exact_real(number_value(args[1]).real)
            : RealNumber::exact(BigInteger(1), BigInteger(1000000000000LL));
        if (tolerance.numerator.negative())
            tolerance.numerator = -tolerance.numerator;
        const bool inexact_input = !number_value(args[0]).is_exact() ||
            (args.size() == 2 && !number_value(args[1]).is_exact());
        RealNumber lower = rational_subtract(x, tolerance);
        RealNumber upper = rational_add(x, tolerance);
        RealNumber result = simplest_rational(std::move(lower),
                                              std::move(upper));
        if (inexact_input)
            return Values{evaluator.number(Number::inexact(result.to_double()))};
        return Values{evaluator.number(Number::complex(
            std::move(result), RealNumber::exact(BigInteger(0))))};
    });
    install(evaluator, "square", [&evaluator](const Values& args) {
        require_arity(args, 1, "square");
        if (!is_number(args[0])) throw std::runtime_error("square expects a number");
        Number n = number_value(args[0]);
        return Values{evaluator.number(number_multiply(n, n))};
    });
    auto install_rounding = [&evaluator](const char* name, int mode) {
        install(evaluator, name, [&evaluator, name, mode](const Values& args) -> Values {
            if (args.size() != 1)
                raise_keyed(evaluator, "wrong-number-of-args",
                            std::string(name) + " expects one argument");
            if (!is_number(args[0]) || !number_value(args[0]).is_real())
                raise_keyed(evaluator, "wrong-type-arg",
                            std::string(name) + " expects a real number");
            RealNumber value = number_value(args[0]).real;
            if (value.inexact) {
                double rounded = mode == 0 ? std::floor(value.inexact_value)
                    : mode == 1 ? std::ceil(value.inexact_value)
                    : mode == 2 ? std::trunc(value.inexact_value)
                    : std::nearbyint(value.inexact_value);
                return Values{evaluator.number(Number::inexact(rounded))};
            }
            BigInteger quotient = value.numerator / value.denominator;
            BigInteger remainder = value.numerator % value.denominator;
            if (mode == 0 && value.numerator.negative() &&
                !remainder.is_zero())
                quotient -= BigInteger(1);
            else if (mode == 1 && !value.numerator.negative() &&
                     !remainder.is_zero())
                quotient += BigInteger(1);
            else if (mode == 3 && !remainder.is_zero()) {
                BigInteger magnitude = remainder.negative()
                    ? -remainder : remainder;
                int half = compare(magnitude * BigInteger(2),
                                   value.denominator);
                const bool odd = (quotient.native() & 1) != 0;
                if (half > 0 || (half == 0 && odd))
                    quotient += value.numerator.negative()
                        ? BigInteger(-1) : BigInteger(1);
            }
            return Values{evaluator.number(Number::exact(std::move(quotient)))};
        });
    };
    install_rounding("floor", 0);
    install_rounding("ceiling", 1);
    install_rounding("truncate", 2);
    install_rounding("round", 3);
    install(evaluator, "gcd", [&evaluator](const Values& args) -> Values {
        auto gcd2 = [](BigInteger a, BigInteger b) {
            if (a.negative()) a = -a;
            if (b.negative()) b = -b;
            while (!b.is_zero()) {
                BigInteger t = a % b;
                a = std::move(b);
                b = std::move(t);
            }
            return a;
        };
        BigInteger result(0);
        bool any_inexact = false;
        for (const Value& argument : args) {
            BigInteger value;
            if (!integer_value(argument, value))
                raise_keyed(evaluator, "wrong-type-arg",
                            "gcd expects integers");
            any_inexact = any_inexact || !number_value(argument).is_exact();
            result = gcd2(std::move(result), std::move(value));
        }
        return Values{make_integer(evaluator, std::move(result), any_inexact)};
    });
    install(evaluator, "lcm", [&evaluator](const Values& args) -> Values {
        auto abs_i = [](BigInteger v) { return v.negative() ? -v : v; };
        auto gcd2 = [](BigInteger a, BigInteger b) {
            while (!b.is_zero()) {
                BigInteger t = a % b;
                a = std::move(b);
                b = std::move(t);
            }
            return a;
        };
        BigInteger result_numerator(1), result_denominator(1);
        bool have_value = false;
        bool any_inexact = false;
        for (const Value& argument : args) {
            if (!is_number(argument) || !number_value(argument).is_real())
                raise_keyed(evaluator, "type-error",
                            "lcm expects real numbers");
            RealNumber value = number_value(argument).real;
            any_inexact = any_inexact || value.inexact;
            if (value.inexact) {
                if (!std::isfinite(value.inexact_value))
                    raise_keyed(evaluator, "type-error",
                                "lcm expects finite real numbers");
                value = exact_from_double(value.inexact_value);
            }
            BigInteger numerator = abs_i(std::move(value.numerator));
            if (!have_value) {
                result_numerator = std::move(numerator);
                result_denominator = std::move(value.denominator);
                have_value = true;
            } else {
                result_numerator = (result_numerator /
                    gcd2(result_numerator, numerator)) * numerator;
                result_denominator = gcd2(result_denominator,
                                          value.denominator);
            }
        }
        Number result = Number::rational(std::move(result_numerator),
                                         std::move(result_denominator));
        if (any_inexact)
            result = Number::inexact(result.real.to_double());
        return Values{evaluator.number(std::move(result))};
    });
    install(evaluator, "exact-integer-sqrt", [&evaluator](const Values& args) -> Values {
        require_arity(args, 1, "exact-integer-sqrt");
        // Split the checks: non-integer -> 'type-error, negative ->
        // 'value-error (the old combined message collapsed both into one
        // classifier bucket).
        BigInteger input;
        if (!integer_value(args[0], input) ||
            !number_value(args[0]).is_exact())
            throw std::runtime_error("exact-integer-sqrt expects integers");
        if (input.negative())
            throw std::runtime_error(
                "exact-integer-sqrt n must be non-negative");
        const auto& n = input.native();
        if (n == 0)
            return Values{Value::integer(0), Value::integer(0)};
        boost::multiprecision::cpp_int root = 1;
        while (root * root <= n) root <<= 1;
        boost::multiprecision::cpp_int next;
        do {
            next = (root + n / root) >> 1;
            if (next >= root) break;
            root = std::move(next);
        } while (true);
        BigInteger root_value(std::move(root));
        BigInteger remainder = input - root_value * root_value;
        return Values{evaluator.number(Number::exact(root_value)),
                      evaluator.number(Number::exact(std::move(remainder)))};
    });
    install(evaluator, "pair?", [](const Values& args) {
        require_arity(args, 1, "pair?");
        return Values{Value::boolean(
            args[0].is_object() && args[0].as_object()->type() == ObjectType::Pair)};
    });
    install(evaluator, "null?", [](const Values& args) {
        require_arity(args, 1, "null?");
        return Values{Value::boolean(args[0].is_null())};
    });
    install(evaluator, "list?", [](const Values& args) {
        require_arity(args, 1, "list?");
        return Values{Value::boolean(is_proper_list(args[0]))};
    });
    install(evaluator, "proper-list?", [](const Values& args) {
        require_arity(args, 1, "proper-list?");
        return Values{Value::boolean(is_proper_list(args[0]))};
    });
    install(evaluator, "list-ref", [](const Values& args) {
        require_arity(args, 2, "list-ref");
        Value rest = args[0];
        std::int64_t index = args[1].as_integer();
        if (index < 0)
            throw std::runtime_error("list-ref index is negative");
        while (index-- > 0) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("list-ref index out of bounds");
            rest = rest.as_object<PairObject>()->cdr;
        }
        if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("list-ref index out of bounds");
        return Values{rest.as_object<PairObject>()->car};
    });
    install(evaluator, "sort", [&evaluator](const Values& args) {
        require_arity(args, 2, "sort");
        std::vector<Value> values;
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("sort expects a proper list");
            values.push_back(rest.as_object<PairObject>()->car);
            rest = rest.as_object<PairObject>()->cdr;
        }
        for (std::size_t i = 1; i < values.size(); ++i) {
            Value item = values[i];
            std::size_t j = i;
            while (j > 0) {
                Value before = evaluator.apply_values(
                    args[0], {item, values[j - 1]})[0];
                if (!before.is_boolean() || !before.as_boolean()) break;
                values[j] = values[j - 1];
                --j;
            }
            values[j] = item;
        }
        return Values{evaluator.list(values)};
    });
    install(evaluator, "symbol?", [](const Values& args) {
        require_arity(args, 1, "symbol?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Symbol)};
    });
    install(evaluator, "keyword?", [](const Values& args) {
        require_arity(args, 1, "keyword?");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Symbol)
            return Values{Value::boolean(false)};
        const auto& name = args[0].as_object<SymbolObject>()->name;
        return Values{Value::boolean(!name.empty() && name.front() == ':')};
    });
    install(evaluator, "string?", [](const Values& args) {
        require_arity(args, 1, "string?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::String)};
    });
    install(evaluator, "vector?", [](const Values& args) {
        require_arity(args, 1, "vector?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Vector)};
    });
    // --- bytevectors: raw byte strings (the substrate has no integer
    // vector).  #u8 literals, the utf8 conversions and (scheme base)'s
    // bytevector API all land here.
    install(evaluator, "bytevector?", [](const Values& args) {
        require_arity(args, 1, "bytevector?");
        return Values{Value::boolean(
            args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::Bytevector)};
    });
    auto bytevector_object = [](Value value, const char* name) -> BytevectorObject* {
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::Bytevector)
            throw std::runtime_error(std::string(name) +
                                     " expects a bytevector");
        return value.as_object<BytevectorObject>();
    };
    auto u8_of = [](Value value, const char* name) -> unsigned char {
        if (!value.is_integer())
            throw std::runtime_error(std::string(name) +
                                     " expects exact integers");
        std::int64_t number = value.as_integer();
        if (number < 0 || number > 255)
            throw std::runtime_error(std::string(name) +
                                     " byte out of range");
        return static_cast<unsigned char>(number);
    };
    install(evaluator, "bytevector", [&evaluator, u8_of](const Values& args) {
        std::string bytes;
        for (const Value& argument : args)
            bytes.push_back(static_cast<char>(u8_of(argument, "bytevector")));
        return Values{Value::object(
            evaluator.heap().make<BytevectorObject>(std::move(bytes)))};
    });
    install(evaluator, "make-bytevector",
            [&evaluator, u8_of](const Values& args) {
                if (args.empty() || args.size() > 2)
                    throw std::runtime_error(
                        "make-bytevector expects a length and optional fill");
                if (!args[0].is_integer() || args[0].as_integer() < 0)
                    throw std::runtime_error(
                        "make-bytevector length must be non-negative");
                std::size_t length =
                    static_cast<std::size_t>(args[0].as_integer());
                unsigned char fill = args.size() == 2
                                         ? u8_of(args[1], "make-bytevector")
                                         : 0;
                return Values{Value::object(evaluator.heap().make<BytevectorObject>(
                    std::string(length, static_cast<char>(fill))))};
            });
    install(evaluator, "bytevector-length", [bytevector_object](const Values& args) {
        require_arity(args, 1, "bytevector-length");
        return Values{Value::integer(static_cast<std::int64_t>(
            bytevector_object(args[0], "bytevector-length")->bytes.size()))};
    });
    install(evaluator, "bytevector-u8-ref", [bytevector_object](const Values& args) {
        require_arity(args, 2, "bytevector-u8-ref");
        auto* bytes = bytevector_object(args[0], "bytevector-u8-ref");
        if (!args[1].is_integer())
            throw std::runtime_error("bytevector-u8-ref index must be an integer");
        std::int64_t index = args[1].as_integer();
        if (index < 0 ||
            static_cast<std::size_t>(index) >= bytes->bytes.size())
            throw std::runtime_error("bytevector-u8-ref index out of bounds");
        return Values{Value::integer(static_cast<unsigned char>(
            bytes->bytes[static_cast<std::size_t>(index)]))};
    });
    install(evaluator, "bytevector-u8-set!", [&evaluator, bytevector_object](const Values& args) {
        require_arity(args, 3, "bytevector-u8-set!");
        auto* bytes = bytevector_object(args[0], "bytevector-u8-set!");
        if (!args[1].is_integer())
            throw std::runtime_error("bytevector-u8-set! index must be an integer");
        std::int64_t index = args[1].as_integer();
        if (index < 0 ||
            static_cast<std::size_t>(index) >= bytes->bytes.size())
            throw std::runtime_error("bytevector-u8-set! index out of bounds");
        if (!args[2].is_integer() || args[2].as_integer() < 0 ||
            args[2].as_integer() > 255)
            raise_keyed(evaluator, "wrong-type-arg",
                        "bytevector-u8-set! value must be an unsigned byte");
        bytes->bytes[static_cast<std::size_t>(index)] =
            static_cast<char>(args[2].as_integer());
        return Values{Value::unspecified()};
    });
    install(evaluator, "bytevector-copy", [&evaluator, bytevector_object](const Values& args) {
        if (args.empty() || args.size() > 3)
            throw std::runtime_error("bytevector-copy expects1 to3 arguments");
        auto* bytes = bytevector_object(args[0], "bytevector-copy");
        std::int64_t length = static_cast<std::int64_t>(bytes->bytes.size());
        std::int64_t start = args.size() >= 2 ? args[1].as_integer() : 0;
        std::int64_t end = args.size() >= 3 ? args[2].as_integer() : length;
        if (start < 0) start = 0;
        if (end > length) end = length;
        if (end < start) end = start;
        return Values{Value::object(evaluator.heap().make<BytevectorObject>(
            bytes->bytes.substr(static_cast<std::size_t>(start),
                                static_cast<std::size_t>(end - start))))};
    });
    install(evaluator, "bytevector-copy!", [bytevector_object](const Values& args) {
        if (args.size() != 3 && args.size() != 5)
            throw std::runtime_error(
                "bytevector-copy! expects (to at from [start [end]])");
        auto* to = bytevector_object(args[0], "bytevector-copy!");
        auto* from = bytevector_object(args[2], "bytevector-copy!");
        if (!args[1].is_integer())
            throw std::runtime_error("bytevector-copy! at must be an integer");
        std::int64_t at = args[1].as_integer();
        std::int64_t length = static_cast<std::int64_t>(from->bytes.size());
        std::int64_t start = args.size() >= 4 ? args[3].as_integer() : 0;
        std::int64_t end = args.size() >= 5 ? args[4].as_integer() : length;
        if (start < 0) start = 0;
        if (end > length) end = length;
        if (end < start) end = start;
        if (at < 0 || at + (end - start) > static_cast<std::int64_t>(to->bytes.size()))
            throw std::runtime_error("bytevector-copy! range out of bounds");
        for (std::int64_t i = start; i < end; ++i)
            to->bytes[static_cast<std::size_t>(at + (i - start))] =
                from->bytes[static_cast<std::size_t>(i)];
        return Values{Value::unspecified()};
    });
    install(evaluator, "bytevector-append", [&evaluator](const Values& args) {
        std::string bytes;
        for (const Value& argument : args) {
            if (!argument.is_object() ||
                argument.as_object()->type() != ObjectType::Bytevector)
                throw std::runtime_error("bytevector-append expects bytevectors");
            bytes += argument.as_object<BytevectorObject>()->bytes;
        }
        return Values{Value::object(
            evaluator.heap().make<BytevectorObject>(std::move(bytes)))};
    });
    install(evaluator, "bytevector->u8-list", [&evaluator, bytevector_object](const Values& args) {
        require_arity(args, 1, "bytevector->u8-list");
        const std::string& bytes =
            bytevector_object(args[0], "bytevector->u8-list")->bytes;
        std::vector<Value> list;
        for (char byte : bytes)
            list.push_back(
                Value::integer(static_cast<unsigned char>(byte)));
        return Values{evaluator.list(list)};
    });
    install(evaluator, "u8-list->bytevector", [&evaluator, u8_of](const Values& args) {
        require_arity(args, 1, "u8-list->bytevector");
        std::string bytes;
        for (Value element : proper_list(args[0]))
            bytes.push_back(static_cast<char>(u8_of(element, "u8-list->bytevector")));
        return Values{Value::object(
            evaluator.heap().make<BytevectorObject>(std::move(bytes)))};
    });
    install(evaluator, "string->utf8", [&evaluator](const Values& args) {
        if (args.empty())
            raise_keyed(evaluator, "type-error", "string->utf8 expects a string");
        if (args.size() > 3)
            throw std::runtime_error(
                "string->utf8 expects one to three arguments");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::String)
            raise_keyed(evaluator, "type-error", "string->utf8 expects a string");
        const std::string& value = args[0].as_object<StringObject>()->value;
        // A full conversion needs validation but no character-index table.
        if (args.size() == 1) {
            if (!utf8_valid(value))
                raise_keyed(evaluator, "value-error",
                            "string->utf8 received invalid UTF-8");
            return Values{Value::object(
                evaluator.heap().make<BytevectorObject>(value))};
        }
        std::vector<std::size_t> offsets;
        if (!utf8_offsets(value, offsets))
            raise_keyed(evaluator, "value-error",
                        "string->utf8 received invalid UTF-8");
        const auto char_count = static_cast<std::int64_t>(offsets.size() - 1);
        std::int64_t start = 0;
        std::int64_t end = char_count;
        if (args.size() >= 2) {
            if (!args[1].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "string->utf8 start must be an integer");
            start = args[1].as_integer();
        }
        if (args.size() == 3) {
            if (!args[2].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "string->utf8 end must be an integer");
            end = args[2].as_integer();
        }
        if (start < 0 || end < start || end > char_count)
            raise_keyed(evaluator, "out-of-range",
                        "string->utf8 index out of range");
        const std::size_t byte_start = offsets[static_cast<std::size_t>(start)];
        const std::size_t byte_end = offsets[static_cast<std::size_t>(end)];
        return Values{Value::object(evaluator.heap().make<BytevectorObject>(
            value.substr(byte_start, byte_end - byte_start)))};
    });
    install(evaluator, "utf8->string", [&evaluator](const Values& args) {
        if (args.empty())
            raise_keyed(evaluator, "wrong-type-arg",
                        "utf8->string expects a bytevector");
        if (args.size() > 3)
            throw std::runtime_error(
                "utf8->string expects one to three arguments");
        std::string bytes;
        if (args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::Bytevector) {
            bytes = args[0].as_object<BytevectorObject>()->bytes;
        } else if (args[0].is_null()) {
            bytes.clear();
        } else if (args[0].is_object() &&
                   args[0].as_object()->type() == ObjectType::Pair) {
            for (Value value : proper_list(args[0])) {
                if (!value.is_integer() || value.as_integer() < 0 ||
                    value.as_integer() > 255)
                    raise_keyed(evaluator, "wrong-type-arg",
                                "utf8->string list elements must be bytes");
                bytes.push_back(static_cast<char>(value.as_integer()));
            }
        } else {
            raise_keyed(evaluator, "wrong-type-arg",
                        "utf8->string expects a bytevector");
        }
        std::int64_t start = 0;
        auto end = static_cast<std::int64_t>(bytes.size());
        if (args.size() >= 2) {
            if (!args[1].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "utf8->string start must be an integer");
            start = args[1].as_integer();
        }
        if (args.size() == 3) {
            if (!args[2].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "utf8->string end must be an integer");
            end = args[2].as_integer();
        }
        if (start < 0 || end < start ||
            static_cast<std::size_t>(end) > bytes.size())
            raise_keyed(evaluator, "out-of-range",
                        "utf8->string index out of range");
        std::string slice = bytes.substr(
            static_cast<std::size_t>(start),
            static_cast<std::size_t>(end - start));
        std::vector<std::size_t> offsets;
        if (!utf8_offsets(slice, offsets))
            raise_keyed(evaluator, "value-error",
                        "utf8->string received invalid UTF-8");
        return Values{evaluator.string(slice)};
    });
    install(evaluator, "utf8-string-length", [](const Values& args) {
        require_arity(args, 1, "utf8-string-length");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::String)
            throw std::runtime_error("utf8-string-length expects a string");
        const std::string& bytes =
            args[0].as_object<StringObject>()->value;
        return Values{Value::integer(static_cast<std::int64_t>(utf8_length(bytes)))};
    });
    install(evaluator, "procedure?", [](const Values& args) {
        require_arity(args, 1, "procedure?");
        bool result = args[0].is_object() &&
                      (args[0].as_object()->type() == ObjectType::Closure ||
                       args[0].as_object()->type() == ObjectType::Primitive ||
                       args[0].as_object()->type() == ObjectType::Continuation);
        return Values{Value::boolean(result)};
    });
    install(evaluator, "type-of", [&evaluator](const Values& args) {
        require_arity(args, 1, "type-of");
        if (args[0].is_null()) return Values{evaluator.symbol("null")};
        if (args[0].is_unspecified())
            return Values{evaluator.symbol("unspecified")};
        if (args[0].is_boolean()) return Values{evaluator.symbol("boolean")};
        if (args[0].is_integer()) return Values{evaluator.symbol("integer")};
        switch (args[0].as_object()->type()) {
        case ObjectType::Number: return Values{evaluator.symbol("number")};
        case ObjectType::Pair: return Values{evaluator.symbol("pair")};
        case ObjectType::Symbol: return Values{evaluator.symbol("symbol")};
        case ObjectType::String: return Values{evaluator.symbol("string")};
        case ObjectType::Vector: return Values{evaluator.symbol("vector")};
        case ObjectType::Character: return Values{evaluator.symbol("character")};
        case ObjectType::Primitive:
        case ObjectType::Closure:
        case ObjectType::Continuation:
            return Values{evaluator.symbol("procedure")};
        default: return Values{evaluator.symbol("object")};
        }
    });
    install(evaluator, "eq?", [](const Values& args) {
        require_arity(args, 2, "eq?");
        return Values{Value::boolean(same(args[0], args[1]))};
    });
    install(evaluator, "eqv?", [](const Values& args) {
        require_arity(args, 2, "eqv?");
        return Values{Value::boolean(equivalent(args[0], args[1]))};
    });
    install(evaluator, "equal?", [](const Values& args) {
        require_arity(args, 2, "equal?");
        return Values{Value::boolean(equal(args[0], args[1]))};
    });
    evaluator.define_machine_primitive("catch", PrimitiveObject::Kind::Catch);
    evaluator.define_machine_primitive("%native-with-exception-handler",
                                      PrimitiveObject::Kind::WithExceptionHandler);
    evaluator.define_machine_primitive("%native-raise-continuable",
                                      PrimitiveObject::Kind::RaiseContinuable);
    install(evaluator, "throw", [](const Values& args) -> Values {
        if (args.empty()) throw std::runtime_error("throw expects a tag");
        ValueList arguments(args.begin() + 1, args.end());
        throw ThrownValue(args[0], std::move(arguments));
    });
    // The kernel lists `error' among the primitive names and (scheme base)
    // reaches it through (rename (goldfish) (error host-error)) for
    // non-string messages, so a bare `error' must exist globally (the host
    // has s7's builtin there).  s7's builtin shape: the first argument is
    // the catch tag -- any value, not just a symbol -- and the rest the
    // irritant list, which is what `throw' does.  String messages never
    // reach here: (scheme base)'s wrapper turns them into error objects.
    install(evaluator, "error", [](const Values& args) -> Values {
        if (args.empty())
            throw std::runtime_error("error expects a message");
        ValueList arguments(args.begin() + 1, args.end());
        throw ThrownValue(args[0], std::move(arguments));
    });
    // s7 source-position accessors (used by srfi-78's report line).  Native
    // keeps no per-form source locations, so every pair answers #f -- the
    // documented "no such number/file available" case.
    install(evaluator, "pair-filename", [](const Values& args) -> Values {
        require_arity(args, 1, "pair-filename");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("pair-filename expects a pair");
        return Values{Value::boolean(false)};
    });
    install(evaluator, "pair-line-number", [](const Values& args) -> Values {
        require_arity(args, 1, "pair-line-number");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("pair-line-number expects a pair");
        return Values{Value::boolean(false)};
    });

    install(evaluator, "cons", [&evaluator](const Values& args) {
        require_arity(args, 2, "cons");
        return Values{evaluator.pair(args[0], args[1])};
    });
    install(evaluator, "car", [](const Values& args) {
        require_arity(args, 1, "car");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("car expects a pair");
        return Values{args[0].as_object<PairObject>()->car};
    });
    install(evaluator, "cdr", [](const Values& args) {
        require_arity(args, 1, "cdr");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("cdr expects a pair");
        return Values{args[0].as_object<PairObject>()->cdr};
    });
    for (const char* name : {"caar", "cadr", "cdar", "cddr",
                             "caaar", "caadr", "cadar", "caddr",
                             "cdaar", "cdadr", "cddar", "cdddr",
                             "caaaar", "caaadr", "caadar", "caaddr",
                             "cadaar", "cadadr", "caddar", "cadddr",
                             "cdaaar", "cdaadr", "cdadar", "cdaddr",
                             "cddaar", "cddadr", "cdddar", "cddddr"}) {
        install(evaluator, name, [name](const Values& args) {
            require_arity(args, 1, name);
            Value value = args[0];
            const char* last = name + std::strlen(name) - 1;
            for (const char* it = last - 1; it >= name + 1; --it) {
                if (!value.is_object() || value.as_object()->type() != ObjectType::Pair)
                    throw std::runtime_error(std::string(name) +
                                             " expects nested pairs");
                auto* pair = value.as_object<PairObject>();
                value = *it == 'a' ? pair->car : pair->cdr;
            }
            return Values{value};
        });
    }
    install(evaluator, "set-car!", [](const Values& args) {
        require_arity(args, 2, "set-car!");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("set-car! expects a pair");
        args[0].as_object<PairObject>()->car = args[1];
        return Values{Value::unspecified()};
    });
    install(evaluator, "set-cdr!", [](const Values& args) {
        require_arity(args, 2, "set-cdr!");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("set-cdr! expects a pair");
        args[0].as_object<PairObject>()->cdr = args[1];
        return Values{Value::unspecified()};
    });
    install(evaluator, "list", [&evaluator](const Values& args) {
        return Values{evaluator.list(args)};
    });
    install(evaluator, "make-vector", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("make-vector expects one or two arguments");
        std::int64_t size = args[0].as_integer();
        if (size < 0)
            throw std::runtime_error("make-vector size is negative");
        Value fill = args.size() == 2 ? args[1] : Value::unspecified();
        return Values{evaluator.vector(std::vector<Value>(
            static_cast<std::size_t>(size), fill))};
    });
    install(evaluator, "vector", [&evaluator](const Values& args) {
        return Values{evaluator.vector(args)};
    });
    install(evaluator, "vector-length", [](const Values& args) {
        require_arity(args, 1, "vector-length");
        return Values{Value::integer(static_cast<std::int64_t>(
            vector_storage(args[0]).size()))};
    });
    install(evaluator, "vector-ref", [](const Values& args) {
        require_arity(args, 2, "vector-ref");
        const auto& values = vector_storage(args[0]);
        std::int64_t index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= values.size())
            throw std::runtime_error("vector-ref index out of bounds: " +
                                     std::to_string(index) + " / " +
                                     std::to_string(values.size()));
        return Values{values[static_cast<std::size_t>(index)]};
    });
    install(evaluator, "vector-set!", [&evaluator](const Values& args) {
        require_arity(args, 3, "vector-set!");
        auto& values = args[0].as_object<VectorObject>()->values;
        std::int64_t index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= values.size())
            throw std::runtime_error("vector-set! index out of bounds");
        values[static_cast<std::size_t>(index)] = args[2];
        return Values{Value::unspecified()};
    });

    install(evaluator, "symbol->string", [&evaluator](const Values& args) {
        require_arity(args, 1, "symbol->string");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Symbol)
            raise_keyed(evaluator, "wrong-type-arg",
                        "symbol->string expects a symbol");
        return Values{evaluator.string(args[0].as_object<SymbolObject>()->name)};
    });
    install(evaluator, "string->symbol", [&evaluator](const Values& args) {
        require_arity(args, 1, "string->symbol");
        return Values{evaluator.symbol(evaluator.string_value(args[0]))};
    });
    install(evaluator, "string->keyword", [&evaluator](const Values& args) {
        require_arity(args, 1, "string->keyword");
        std::string name = evaluator.string_value(args[0]);
        if (name.empty() || name.front() != ':') name.insert(name.begin(), ':');
        return Values{evaluator.symbol(name)};
    });
    install(evaluator, "keyword->symbol", [&evaluator](const Values& args) {
        require_arity(args, 1, "keyword->symbol");
        const std::string& name = args[0].as_object<SymbolObject>()->name;
        return Values{evaluator.symbol(name.size() > 0 && name.front() == ':'
                                             ? name.substr(1) : name)};
    });
    install(evaluator, "symbol->keyword", [&evaluator](const Values& args) {
        require_arity(args, 1, "symbol->keyword");
        std::string name = args[0].as_object<SymbolObject>()->name;
        if (name.empty() || name.front() != ':') name.insert(name.begin(), ':');
        return Values{evaluator.symbol(name)};
    });
    install(evaluator, "number->string", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 2)
            throw std::runtime_error("number->string expects one or two arguments");
        if (!is_number(args[0])) throw std::runtime_error("number->string expects a number");
        unsigned radix = 10;
        if (args.size() == 2) {
            BigInteger integer_radix;
            if (!integer_value(args[1], integer_radix) ||
                !number_value(args[1]).is_exact())
                throw std::runtime_error("number->string expects an exact integer radix");
            if (integer_radix != BigInteger(2) &&
                integer_radix != BigInteger(8) &&
                integer_radix != BigInteger(10) &&
                integer_radix != BigInteger(16))
                throw std::runtime_error("number->string radix out of range");
            radix = static_cast<unsigned>(integer_radix.to_int64());
        }
        Number n = number_value(args[0]);
        if (radix != 10 && !n.is_exact())
            throw std::runtime_error("non-decimal number->string requires an exact number");
        return Values{evaluator.string(number_to_string(args[0], radix))};
    });
    install(evaluator, "string->number", [&evaluator](const Values& args) {
        if (args.size() < 1 || args.size() > 2)
            throw std::runtime_error(
                "string->number expects one or two arguments");
        const std::string text = evaluator.string_value(args[0]);
        unsigned radix = 10;
        if (args.size() == 2) {
            BigInteger integer_radix;
            if (!integer_value(args[1], integer_radix) ||
                !number_value(args[1]).is_exact())
                throw std::runtime_error(
                    "string->number expects an exact integer radix");
            if (integer_radix != BigInteger(2) &&
                integer_radix != BigInteger(8) &&
                integer_radix != BigInteger(10) &&
                integer_radix != BigInteger(16))
                raise_keyed(evaluator, "out-of-range",
                            "string->number radix out of range");
            radix = static_cast<unsigned>(integer_radix.to_int64());
        }
        Number result;
        if (!parse_number(text, result, radix)) return Values{Value::boolean(false)};
        return Values{evaluator.number(std::move(result))};
    });
    install(evaluator, "string-length", [&evaluator](const Values& args) {
        require_arity(args, 1, "string-length");
        return Values{Value::integer(static_cast<std::int64_t>(
            utf8_length(evaluator.string_value(args[0]))))};
    });
    install(evaluator, "string-append", [&evaluator](const Values& args) {
        std::string result;
        for (Value arg : args)
            result += evaluator.string_value(arg);
        return Values{evaluator.string(result)};
    });
    install(evaluator, "string-copy", [&evaluator](const Values& args) {
        // R7RS (string-copy s [start [end]]) plus the host's two-arg
        // (string-copy s start) form.
        if (args.size() < 1 || args.size() > 3)
            throw std::runtime_error("string-copy expects one or three arguments");
        const std::string& value = evaluator.string_value(args[0]);
        const std::size_t start = args.size() >= 2
            ? static_cast<std::size_t>(args[1].as_integer()) : 0;
        const std::size_t end = args.size() == 3
            ? static_cast<std::size_t>(args[2].as_integer()) : utf8_length(value);
        if (start > end || end > utf8_length(value))
            throw std::runtime_error("string-copy index out of bounds");
        const auto byte_start = utf8_byte_offset(value, start);
        const auto byte_end = utf8_byte_offset(value, end);
        return Values{evaluator.string(value.substr(byte_start, byte_end - byte_start))};
    });
    install(evaluator, "string->list", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 3)
            throw std::runtime_error(
                "string->list expects one to three arguments");
        const std::string& value = evaluator.string_value(args[0]);
        std::int64_t start = 0;
        auto end = static_cast<std::int64_t>(utf8_length(value));
        if (args.size() >= 2) {
            if (!args[1].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "string->list start must be an integer");
            start = args[1].as_integer();
        }
        if (args.size() == 3) {
            if (!args[2].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "string->list end must be an integer");
            end = args[2].as_integer();
        }
        if (start < 0 || end < start ||
            static_cast<std::size_t>(end) > utf8_length(value))
            throw std::runtime_error(
                "out-of-range: string->list index out of bounds");
        std::vector<Value> chars;
        auto position = utf8_byte_offset(value, start);
        for (auto i = start; i < end; ++i) {
            std::size_t width = 0;
            chars.push_back(evaluator.character(utf8_character_at(value, position, width)));
            position += width;
        }
        return Values{evaluator.list(chars)};
    });
    install(evaluator, "list->string", [&evaluator](const Values& args) {
        require_arity(args, 1, "list->string");
        std::string result;
        for (Value character : proper_list(args[0])) {
            if (!character.is_object() ||
                character.as_object()->type() != ObjectType::Character)
                raise_keyed(evaluator, "wrong-type-arg",
                            "list->string elements must be characters");
            result += utf8_encode_char(evaluator.character_value(character));
        }
        return Values{evaluator.string(result)};
    });
    install(evaluator, "make-string", [&evaluator](const Values& args) {
        if (args.size() < 1 || args.size() > 2)
            throw std::runtime_error(
                "make-string expects one or two arguments");
        if (!args[0].is_integer())
            throw std::runtime_error("make-string length must be an integer");
        const std::int64_t length = args[0].as_integer();
        if (length < 0)
            throw std::runtime_error("out-of-range: make-string length is negative");
        std::string unit(1, '\0'); // R7RS leaves the initial contents impl-defined
        if (args.size() == 2) {
            if (!args[1].is_object() ||
                args[1].as_object()->type() != ObjectType::Character)
                throw std::runtime_error(
                    "wrong-type-arg: make-string fill must be a character");
            unit = utf8_encode_char(args[1].as_object<CharacterObject>()->value);
            if (unit.empty()) unit = std::string(1, '\0');
        }
        std::string text;
        text.reserve(unit.size() * static_cast<std::size_t>(length));
        for (std::int64_t i = 0; i < length; ++i) text += unit;
        return Values{evaluator.string(text)};
    });
    install(evaluator, "string-set!", [&evaluator](const Values& args) {
        require_arity(args, 3, "string-set!");
        if (!args[2].is_object() || args[2].as_object()->type() != ObjectType::Character)
            throw std::runtime_error("wrong-type-arg: string-set! expects a character");
        (void)evaluator.string_value(args[0]);
        auto& value = args[0].as_object<StringObject>()->value;
        const auto index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= utf8_length(value))
            throw std::runtime_error("string-set! index out of bounds");
        const auto position = utf8_byte_offset(value, index);
        std::size_t width = 0;
        utf8_character_at(value, position, width);
        value.replace(position, width, utf8_encode_char(evaluator.character_value(args[2])));
        return Values{Value::unspecified()};
    });
    install(evaluator, "string-copy!", [&evaluator](const Values& args) {
        if (args.size() < 3 || args.size() > 5)
            throw std::runtime_error("string-copy! expects three to five arguments");
        (void)evaluator.string_value(args[0]);
        auto& target = args[0].as_object<StringObject>()->value;
        const auto target_start = args[1].as_integer();
        const std::string& source = evaluator.string_value(args[2]);
        const auto source_start = args.size() >= 4 ? args[3].as_integer() : 0;
        const auto source_end = args.size() == 5
            ? args[4].as_integer() : static_cast<std::int64_t>(utf8_length(source));
        if (target_start < 0 || source_start < 0 || source_end < source_start ||
            source_end > static_cast<std::int64_t>(utf8_length(source)) ||
            target_start > static_cast<std::int64_t>(utf8_length(target)) ||
            source_end - source_start >
                static_cast<std::int64_t>(utf8_length(target)) - target_start)
            throw std::runtime_error("string-copy! index out of bounds");
        const auto from = utf8_byte_offset(source, source_start);
        const auto until = utf8_byte_offset(source, source_end);
        const std::string copied = source.substr(from, until - from);
        const auto to = utf8_byte_offset(target, target_start);
        const auto to_end = utf8_byte_offset(target, target_start + source_end - source_start);
        target.replace(to, to_end - to, copied);
        return Values{args[0]};
    });
    install(evaluator, "string-fill!", [&evaluator](const Values& args) {
        if (args.size() < 2 || args.size() > 4)
            throw std::runtime_error(
                "string-fill! expects two to four arguments");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::String)
            raise_keyed(evaluator, "wrong-type-arg",
                        "string-fill!: first argument must be a string");
        if (!args[1].is_object() ||
            args[1].as_object()->type() != ObjectType::Character)
            raise_keyed(evaluator, "wrong-type-arg",
                        "string-fill!: second argument must be a character");
        auto& value = args[0].as_object<StringObject>()->value;
        const std::string character = utf8_encode_char(evaluator.character_value(args[1]));
        std::int64_t start = 0;
        auto end = static_cast<std::int64_t>(utf8_length(value));
        if (args.size() >= 3) {
            if (!args[2].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "string-fill! start must be an integer");
            start = args[2].as_integer();
        }
        if (args.size() == 4) {
            if (!args[3].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                            "string-fill! end must be an integer");
            end = args[3].as_integer();
        }
        if (start < 0 || end < start ||
            static_cast<std::size_t>(end) > utf8_length(value))
            throw std::runtime_error(
                "out-of-range: string-fill! index out of bounds");
        const auto byte_start = utf8_byte_offset(value, start);
        const auto byte_end = utf8_byte_offset(value, end);
        std::string filled;
        for (auto i = start; i < end; ++i) filled += character;
        value.replace(byte_start, byte_end - byte_start, filled);
        return Values{Value::unspecified()};
    });
    evaluator.define_machine_primitive(
        "string-for-each", PrimitiveObject::Kind::StringForEach);
    for (const auto& entry : {std::pair<const char*, bool(*)(const std::string&, const std::string&)>{
                                  "string=?", [](const auto& a, const auto& b) { return a == b; }},
                              {"string<?", [](const auto& a, const auto& b) { return a < b; }},
                              {"string>?", [](const auto& a, const auto& b) { return a > b; }},
                              {"string<=?", [](const auto& a, const auto& b) { return a <= b; }},
                              {"string>=?", [](const auto& a, const auto& b) { return a >= b; }}}) {
        install(evaluator, entry.first, [name = entry.first, compare = entry.second,
                                         &evaluator](const Values& args) {
            if (args.size() < 2)
                throw std::runtime_error(std::string(name) + " expects two arguments");
            for (std::size_t i = 1; i < args.size(); ++i)
                if (!compare(evaluator.string_value(args[i - 1]),
                             evaluator.string_value(args[i])))
                    return Values{Value::boolean(false)};
            return Values{Value::boolean(true)};
        });
    }
    install(evaluator, "format", [&evaluator](const Values& args) {
        if (args.size() < 2 ||
            (!args[0].is_boolean() &&
             !(args[0].is_object() &&
               args[0].as_object()->type() == ObjectType::String)) ||
            !args[1].is_object() ||
            args[1].as_object()->type() != ObjectType::String)
            throw std::runtime_error("format expects a destination and string");
        const std::string pattern = evaluator.string_value(args[1]);
        std::string result;
        std::size_t argument = 2;
        for (std::size_t i = 0; i < pattern.size(); ++i) {
            if (pattern[i] != '~' || i + 1 >= pattern.size()) {
                result += pattern[i];
                continue;
            }
            char directive = pattern[++i];
            if (directive == '~') {
                result += '~';
            } else if (directive == '%') {
                result += '\n';
            } else if (directive == 'a' || directive == 'A' ||
                       directive == 's' || directive == 'S') {
                if (argument >= args.size())
                    throw std::runtime_error("format missing argument");
                // ~A displays, ~S writes.
                const bool write_mode =
                    directive == 's' || directive == 'S';
                result += format_value(evaluator, args[argument++],
                                       write_mode, 0);
            } else if (directive == 'c' || directive == 'C') {
                if (argument >= args.size())
                    throw std::runtime_error("format missing argument");
                const Value value = args[argument++];
                if (!value.is_object() ||
                    value.as_object()->type() != ObjectType::Character)
                    throw std::runtime_error("format ~C expects a character");
                result += utf8_encode_char(evaluator.character_value(value));
            } else if (directive == 'd' || directive == 'D' ||
                       directive == 'x' || directive == 'X' ||
                       directive == 'b' || directive == 'B' ||
                       directive == 'o' || directive == 'O') {
                if (argument >= args.size())
                    throw std::runtime_error("format missing argument");
                const Value value = args[argument++];
                if (!value.is_integer())
                    throw std::runtime_error("format radix directive expects an integer");
                unsigned int radix = 10;
                if (directive == 'x' || directive == 'X') radix = 16;
                if (directive == 'b' || directive == 'B') radix = 2;
                if (directive == 'o' || directive == 'O') radix = 8;
                std::int64_t integer = value.as_integer();
                const bool negative = integer < 0;
                std::uint64_t magnitude = negative
                    ? static_cast<std::uint64_t>(-(integer + 1)) + 1
                    : static_cast<std::uint64_t>(integer);
                std::string digits;
                do {
                    const unsigned int digit =
                        static_cast<unsigned int>(magnitude % radix);
                    digits.push_back(static_cast<char>(
                        digit < 10 ? '0' + digit : 'a' + digit - 10));
                    magnitude /= radix;
                } while (magnitude != 0);
                if (negative) digits.push_back('-');
                std::reverse(digits.begin(), digits.end());
                result += digits;
            } else {
                result += '~';
                result += directive;
            }
        }
        // String destinations are not runtime ports yet.  Returning the
        // formatted string also covers the #f destination used by bootstrap.
        return Values{evaluator.string(result)};
    });
    install(evaluator, "string-ref", [&evaluator](const Values& args) {
        require_arity(args, 2, "string-ref");
        const std::string& value = evaluator.string_value(args[0]);
        std::int64_t index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= utf8_length(value))
            throw std::runtime_error("string-ref index out of bounds");
        std::size_t width = 0;
        return Values{evaluator.character(
            utf8_character_at(value, utf8_byte_offset(value, index), width))};
    });
    install(evaluator, "substring", [&evaluator](const Values& args) {
        // The host's substring is (str start [end]) -- s7 accepts the
        // two-argument form as "start to the end of the string", which
        // goldtest's suffix matching (and srfi-13, define-star) rely on.
        if (args.size() < 2 || args.size() > 3)
            raise_keyed(evaluator, "wrong-number-of-args",
                "substring expects two or three arguments");
        const std::string& value = evaluator.string_value(args[0]);
        if (!args[1].is_integer())
            raise_keyed(evaluator, "wrong-type-arg",
                "wrong-type-arg: substring start must be an integer");
        auto start = args[1].as_integer();
        auto end = static_cast<std::int64_t>(utf8_length(value));
        if (args.size() == 3) {
            if (!args[2].is_integer())
                raise_keyed(evaluator, "wrong-type-arg",
                    "wrong-type-arg: substring end must be an integer");
            end = args[2].as_integer();
        }
        if (start < 0 || end < start ||
            static_cast<std::size_t>(end) > utf8_length(value))
            raise_keyed(evaluator, "out-of-range",
                "out-of-range: substring index out of bounds");
        const auto byte_start = utf8_byte_offset(value, start);
        const auto byte_end = utf8_byte_offset(value, end);
        return Values{evaluator.string(value.substr(byte_start, byte_end - byte_start))};
    });
    install(evaluator, "char->integer", [&evaluator](const Values& args) {
        require_arity(args, 1, "char->integer");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Character)
            raise_keyed(evaluator, "wrong-type-arg",
                        "char->integer expects a character");
        return Values{Value::integer(static_cast<std::int64_t>(
            evaluator.character_value(args[0])))};
    });
    install(evaluator, "integer->char", [&evaluator](const Values& args) {
        require_arity(args, 1, "integer->char");
        return Values{evaluator.character(static_cast<char32_t>(
            args[0].as_integer()))};
    });
    install(evaluator, "char?", [](const Values& args) {
        require_arity(args, 1, "char?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Character)};
    });
    install_unicode_primitives(evaluator);
    for (const auto& entry : {std::pair<const char*, bool(*)(char32_t, char32_t)>{
                                  "char=?", [](char32_t a, char32_t b) { return a == b; }},
                              {"char<?", [](char32_t a, char32_t b) { return a < b; }},
                              {"char>?", [](char32_t a, char32_t b) { return a > b; }},
                              {"char<=?", [](char32_t a, char32_t b) { return a <= b; }},
                              {"char>=?", [](char32_t a, char32_t b) { return a >= b; }}}) {
        install(evaluator, entry.first, [name = entry.first, compare = entry.second,
                                         &evaluator](const Values& args) {
            if (args.size() < 2)
                throw std::runtime_error(std::string(name) + " expects two arguments");
            for (Value arg : args)
                if (!arg.is_object() ||
                    arg.as_object()->type() != ObjectType::Character)
                    raise_keyed(evaluator, "wrong-type-arg",
                                std::string(name) + " expects characters");
            for (std::size_t i = 1; i < args.size(); ++i)
                if (!compare(evaluator.character_value(args[i - 1]),
                             evaluator.character_value(args[i])))
                    return Values{Value::boolean(false)};
            return Values{Value::boolean(true)};
        });
    }

    // Arithmetic atoms.
    install(evaluator, "+", [&evaluator](const Values& args) {
        Number result = Number::exact(BigInteger(0));
        for (Value arg : args) {
            if (!is_number(arg)) throw std::runtime_error("+ expects numbers");
            result = number_add(result, number_value(arg));
        }
        return Values{evaluator.number(std::move(result))};
    });
    install(evaluator, "-", [&evaluator](const Values& args) {
        if (args.empty()) throw std::runtime_error("- expects arguments");
        if (!is_number(args[0])) throw std::runtime_error("- expects numbers");
        Number result = number_value(args[0]);
        if (args.size() == 1) result = number_negate(result);
        for (std::size_t i = 1; i < args.size(); ++i) {
            if (!is_number(args[i])) throw std::runtime_error("- expects numbers");
            result = number_subtract(result, number_value(args[i]));
        }
        return Values{evaluator.number(std::move(result))};
    });
    install(evaluator, "*", [&evaluator](const Values& args) {
        Number result = Number::exact(BigInteger(1));
        for (Value arg : args) {
            if (!is_number(arg)) throw std::runtime_error("* expects numbers");
            result = number_multiply(result, number_value(arg));
        }
        return Values{evaluator.number(std::move(result))};
    });
    install(evaluator, "=", [](const Values& args) {
        if (args.size() < 2) throw std::runtime_error("= expects two arguments");
        for (Value value : args) if (!is_number(value)) throw std::runtime_error("= expects numbers");
        for (std::size_t i = 1; i < args.size(); ++i)
            if (!number_equal(number_value(args[i]), number_value(args[0]))) return Values{Value::boolean(false)};
        return Values{Value::boolean(true)};
    });
    install(evaluator, "modulo", [&evaluator](const Values& args) {
        require_arity(args, 2, "modulo");
        // Key parity: non-integers -> 'type-error, zero divisor ->
        // 'division-by-zero (tests/scheme pins both).
        BigInteger dividend, divisor;
        if (!integer_value(args[0], dividend) ||
            !integer_value(args[1], divisor))
            throw std::runtime_error("modulo expects integers");
        if (divisor.is_zero())
            throw std::runtime_error("modulo divisor is zero");
        // R7RS floor semantics: the result takes the divisor's sign, so a
        // truncated remainder with mismatched signs gets adjusted (the
        // negative-dividend case below was already correct; (modulo 10
        // -3) must be -2, not C's 1).
        BigInteger result = dividend % divisor;
        if (!result.is_zero() && (result.negative() != divisor.negative()))
            result += divisor;
        return Values{make_integer(evaluator, std::move(result),
                                   number_value(args[0]).real.inexact ||
                                   number_value(args[1]).real.inexact)};
    });
    install(evaluator, "positive?", [](const Values& args) {
        require_arity(args, 1, "positive?");
        if (!is_number(args[0]) || !number_value(args[0]).is_real()) throw std::runtime_error("positive? expects a real number");
        return Values{Value::boolean(number_compare(number_value(args[0]), Number::exact(BigInteger(0))) > 0)};
    });
    install(evaluator, "negative?", [](const Values& args) {
        require_arity(args, 1, "negative?");
        if (!is_number(args[0]) || !number_value(args[0]).is_real())
            throw std::runtime_error("negative? expects a real number");
        Number value = number_value(args[0]);
        if (value.real.inexact && std::isnan(value.real.inexact_value))
            return Values{Value::boolean(false)};
        return Values{Value::boolean(number_compare(
            value, Number::exact(BigInteger(0))) < 0)};
    });
    install(evaluator, "zero?", [](const Values& args) {
        require_arity(args, 1, "zero?");
        if (!is_number(args[0]))
            throw std::runtime_error("zero? expects a number");
        return Values{Value::boolean(number_value(args[0]).is_zero())};
    });
    install(evaluator, "quotient", [&evaluator](const Values& args) {
        require_arity(args, 2, "quotient");
        if (!is_number(args[0]) || !number_value(args[0]).is_real() ||
            !is_number(args[1]) || !number_value(args[1]).is_real())
            raise_keyed(evaluator, "wrong-type-arg",
                        "quotient expects real numbers");
        Number dividend = number_value(args[0]);
        Number divisor = number_value(args[1]);
        if (divisor.is_zero()) throw std::runtime_error("quotient divisor is zero");
        return Values{make_integer(evaluator,
            truncate_real_quotient(dividend, divisor))};
    });
    install(evaluator, "remainder", [&evaluator](const Values& args) {
        require_arity(args, 2, "remainder");
        if (!is_number(args[0]) || !number_value(args[0]).is_real() ||
            !is_number(args[1]) || !number_value(args[1]).is_real())
            raise_keyed(evaluator, "wrong-type-arg",
                        "remainder expects real numbers");
        Number dividend = number_value(args[0]);
        Number divisor = number_value(args[1]);
        if (divisor.is_zero()) throw std::runtime_error("remainder divisor is zero");
        BigInteger quotient = truncate_real_quotient(dividend, divisor);
        return Values{evaluator.number(number_subtract(dividend,
            number_multiply(Number::exact(std::move(quotient)), divisor)))};
    });
    install(evaluator, "sqrt", [&evaluator](const Values& args) {
        require_arity(args, 1, "sqrt");
        if (!is_number(args[0])) throw std::runtime_error("sqrt expects a number");
        return Values{evaluator.number(number_sqrt(number_value(args[0])))};
    });
    install(evaluator, "real-part", [&evaluator](const Values& args) {
        require_arity(args, 1, "real-part");
        if (!is_number(args[0])) throw std::runtime_error("real-part expects a number");
        return Values{evaluator.number(Number::complex(
            number_value(args[0]).real, RealNumber::exact(BigInteger(0))))};
    });
    install(evaluator, "imag-part", [&evaluator](const Values& args) {
        require_arity(args, 1, "imag-part");
        if (!is_number(args[0])) throw std::runtime_error("imag-part expects a number");
        Number value = number_value(args[0]);
        RealNumber imag = value.has_imaginary_part ? value.imag
            : RealNumber::exact(BigInteger(0));
        return Values{evaluator.number(Number::complex(
            std::move(imag), RealNumber::exact(BigInteger(0))))};
    });
    install(evaluator, "make-rectangular", [&evaluator](const Values& args) {
        require_arity(args, 2, "make-rectangular");
        if (!is_number(args[0]) || !is_number(args[1]) ||
            !number_value(args[0]).is_real() || !number_value(args[1]).is_real())
            raise_keyed(evaluator, "wrong-type-arg",
                        "make-rectangular expects real numbers");
        return Values{evaluator.number(Number::complex(
            number_value(args[0]).real, number_value(args[1]).real))};
    });
    install(evaluator, "make-polar", [&evaluator](const Values& args) {
        require_arity(args, 2, "make-polar");
        if (!is_number(args[0]) || !is_number(args[1]) ||
            !number_value(args[0]).is_real() || !number_value(args[1]).is_real())
            raise_keyed(evaluator, "wrong-type-arg",
                        "make-polar expects real numbers");
        double magnitude = number_value(args[0]).real.to_double();
        double angle = number_value(args[1]).real.to_double();
        return Values{evaluator.number(Number::complex(
            RealNumber::inexact_real(magnitude * std::cos(angle)),
            RealNumber::inexact_real(magnitude * std::sin(angle))))};
    });
    install(evaluator, "magnitude", [&evaluator](const Values& args) {
        require_arity(args, 1, "magnitude");
        if (!is_number(args[0])) throw std::runtime_error("magnitude expects a number");
        return Values{evaluator.number(number_abs(number_value(args[0])))};
    });
    install(evaluator, "angle", [&evaluator](const Values& args) {
        require_arity(args, 1, "angle");
        if (!is_number(args[0])) throw std::runtime_error("angle expects a number");
        Number value = number_value(args[0]);
        return Values{evaluator.number(Number::inexact(std::atan2(
            value.imag.to_double(), value.real.to_double())))};
    });
    auto install_complex_unary = [&evaluator](const char* name,
            std::complex<double> (*operation)(const std::complex<double>&)) {
        install(evaluator, name, [name, operation, &evaluator](const Values& args) {
            require_arity(args, 1, name);
            if (!is_number(args[0]))
                throw std::runtime_error(std::string(name) + " expects a number");
            Number value = number_value(args[0]);
            if (value.is_real()) {
                const double x = value.real.to_double();
                const bool real_domain =
                    (std::string(name) != "asin" &&
                     std::string(name) != "acos") ||
                    (x >= -1.0 && x <= 1.0);
                if (real_domain) {
                    double result = std::string(name) == "exp" ? std::exp(x)
                        : std::string(name) == "sin" ? std::sin(x)
                        : std::string(name) == "cos" ? std::cos(x)
                        : std::string(name) == "tan" ? std::tan(x)
                        : std::string(name) == "asin" ? std::asin(x)
                        : std::acos(x);
                    return Values{evaluator.number(Number::inexact(result))};
                }
            }
            std::complex<double> result = operation(std::complex<double>(
                value.real.to_double(), value.imag.to_double()));
            return Values{evaluator.number(Number::complex(
                RealNumber::inexact_real(result.real()),
                RealNumber::inexact_real(result.imag())))};
        });
    };
    install_complex_unary("exp", static_cast<std::complex<double>(*) (
        const std::complex<double>&)>(std::exp<double>));
    install(evaluator, "log", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 2)
            throw std::runtime_error("log expects one or two arguments");
        for (Value arg : args)
            if (!is_number(arg))
                throw std::runtime_error("log expects numbers");
        Number value = number_value(args[0]);
        if (args.size() == 1 && value.is_zero())
            return Values{evaluator.number(Number::complex(
                RealNumber::inexact_real(-std::numeric_limits<double>::infinity()),
                RealNumber::inexact_real(std::acos(-1.0))))};
        if (args.size() == 1 && value.is_exact() && value.is_real() &&
            number_compare(value, Number::exact(BigInteger(1))) == 0)
            return Values{evaluator.number(Number::exact(BigInteger(0)))};
        if (args.size() == 2) {
            Number base = number_value(args[1]);
            if (base.is_real() && number_compare(base,
                    Number::exact(BigInteger(0))) <= 0)
                raise_keyed(evaluator, "out-of-range",
                            "log base must be positive");
            if (value.is_real() && number_compare(value,
                    Number::exact(BigInteger(0))) <= 0)
                raise_keyed(evaluator, "out-of-range",
                            "log argument must be positive");
            if (base.is_real() && base.is_exact() &&
                number_equal(base, Number::exact(BigInteger(1)))) {
                if (value.is_real() && number_equal(
                        value, Number::exact(BigInteger(1))))
                    return Values{evaluator.number(Number::inexact(
                        std::numeric_limits<double>::quiet_NaN()))};
                return Values{evaluator.number(Number::inexact(
                    number_compare(value, Number::exact(BigInteger(1))) > 0
                        ? std::numeric_limits<double>::infinity()
                        : -std::numeric_limits<double>::infinity()))};
            }
            if (value.is_exact() && base.is_exact() && value.is_real() &&
                base.is_real() && value.real.numerator > BigInteger(0) &&
                base.real.numerator > BigInteger(0)) {
                Number inverse = number_divide(Number::exact(BigInteger(1)), base);
                for (int denominator = 1; denominator <= 8; ++denominator) {
                    Number target = Number::exact(BigInteger(1));
                    for (int i = 0; i < denominator; ++i)
                        target = number_multiply(target, value);
                    Number power = Number::exact(BigInteger(1));
                    for (int exponent = 0; exponent <= 64; ++exponent) {
                        if (number_equal(power, target))
                            return Values{evaluator.number(Number::rational(
                                BigInteger(exponent), BigInteger(denominator)))};
                        power = number_multiply(power, base);
                    }
                    power = Number::exact(BigInteger(1));
                    for (int exponent = 1; exponent <= 64; ++exponent) {
                        power = number_multiply(power, inverse);
                        if (number_equal(power, target))
                            return Values{evaluator.number(Number::rational(
                                BigInteger(-exponent), BigInteger(denominator)))};
                    }
                }
            }
            std::complex<double> numerator = std::log(std::complex<double>(
                value.real.to_double(), value.imag.to_double()));
            std::complex<double> denominator = std::log(std::complex<double>(
                base.real.to_double(), base.imag.to_double()));
            std::complex<double> result = numerator / denominator;
            if (value.is_real() && base.is_real() && result.imag() == 0.0)
                return Values{evaluator.number(Number::inexact(result.real()))};
            return Values{evaluator.number(Number::complex(
                RealNumber::inexact_real(result.real()),
                RealNumber::inexact_real(result.imag())))};
        }
        std::complex<double> result = std::log(std::complex<double>(
            value.real.to_double(), value.imag.to_double()));
        if (value.is_real() && result.imag() == 0.0)
            return Values{evaluator.number(Number::inexact(result.real()))};
        return Values{evaluator.number(Number::complex(
            RealNumber::inexact_real(result.real()),
            RealNumber::inexact_real(result.imag())))};
    });
    install_complex_unary("sin", static_cast<std::complex<double>(*) (
        const std::complex<double>&)>(std::sin<double>));
    install_complex_unary("cos", static_cast<std::complex<double>(*) (
        const std::complex<double>&)>(std::cos<double>));
    install_complex_unary("tan", static_cast<std::complex<double>(*) (
        const std::complex<double>&)>(std::tan<double>));
    install_complex_unary("asin", static_cast<std::complex<double>(*) (
        const std::complex<double>&)>(std::asin<double>));
    install_complex_unary("acos", static_cast<std::complex<double>(*) (
        const std::complex<double>&)>(std::acos<double>));
    install(evaluator, "atan", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 2)
            throw std::runtime_error("atan expects one or two arguments");
        for (Value arg : args)
            if (!is_number(arg)) throw std::runtime_error("atan expects numbers");
        if (args.size() == 2) {
            Number y = number_value(args[0]), x = number_value(args[1]);
            if (!y.is_real() || !x.is_real())
                throw std::runtime_error("atan expects real numbers with two arguments");
            return Values{evaluator.number(Number::inexact(std::atan2(
                y.real.to_double(), x.real.to_double())))};
        }
        Number value = number_value(args[0]);
        std::complex<double> result = std::atan(std::complex<double>(
            value.real.to_double(), value.imag.to_double()));
        if (value.is_real() && result.imag() == 0.0)
            return Values{evaluator.number(Number::inexact(result.real()))};
        return Values{evaluator.number(Number::complex(
            RealNumber::inexact_real(result.real()),
            RealNumber::inexact_real(result.imag())))};
    });
    install(evaluator, "/", [&evaluator](const Values& args) {
        if (args.empty()) throw std::runtime_error("/ expects arguments");
        for (Value value : args) if (!is_number(value)) throw std::runtime_error("/ expects numbers");
        Number result = args.size() == 1 ? Number::exact(BigInteger(1)) : number_value(args[0]);
        if (args.size() == 1) result = number_divide(result, number_value(args[0]));
        for (std::size_t i = 1; i < args.size(); ++i) {
            result = number_divide(result, number_value(args[i]));
        }
        return Values{evaluator.number(std::move(result))};
    });
    auto install_comparison = [&evaluator](const char* name, int direction) {
        install(evaluator, name, [name, direction, &evaluator](const Values& args) {
            if (args.size() < 2) throw std::runtime_error(std::string(name) + " expects two arguments");
            for (std::size_t i = 0; i < args.size(); ++i)
                if (!is_number(args[i]) || !number_value(args[i]).is_real())
                    raise_keyed(evaluator, "wrong-type-arg",
                                std::string(name) + " expects real numbers");
            for (std::size_t i = 1; i < args.size(); ++i) {
                Number left = number_value(args[i - 1]);
                Number right = number_value(args[i]);
                if ((left.real.inexact && std::isnan(left.real.inexact_value)) ||
                    (right.real.inexact && std::isnan(right.real.inexact_value)))
                    return Values{Value::boolean(false)};
                int order = number_compare(left, right);
                bool ok = direction == -2 ? order < 0 : direction == -1 ? order <= 0
                    : direction == 1 ? order > 0 : order >= 0;
                if (!ok)
                    return Values{Value::boolean(false)};
            }
            return Values{Value::boolean(true)};
        });
    };
    install_comparison("<", -2);
    install_comparison("<=", -1);
    install_comparison(">", 1);
    install_comparison(">=", 2);

    // s7's copy: in the kernel primitive table, but not a Scheme-level
    // definition anywhere, so the native substrate has to provide it.
    install(evaluator, "copy", [&evaluator](const Values& args) {
        return Values{copy_value(evaluator, args)};
    });

    // (liii string) aliases string-split straight onto this primitive; the
    // host supplies it from liii_string.cpp (string or character separator,
    // empty separator splits per UTF-8 character, trailing empties kept).
    install(evaluator, "g_string-split", [&evaluator](const Values& args) {
        require_arity(args, 2, "g_string-split");
        const std::string text = evaluator.string_value(args[0]);
        std::string separator;
        if (args[1].is_object() &&
            args[1].as_object()->type() == ObjectType::String) {
            separator = evaluator.string_value(args[1]);
        } else if (args[1].is_object() &&
                   args[1].as_object()->type() == ObjectType::Character) {
            const char32_t cp = args[1].as_object<CharacterObject>()->value;
            if (cp < 0x80)
                separator.push_back(static_cast<char>(cp));
            else if (cp < 0x800) {
                separator.push_back(static_cast<char>(0xc0 | (cp >> 6)));
                separator.push_back(static_cast<char>(0x80 | (cp & 0x3f)));
            } else if (cp < 0x10000) {
                separator.push_back(static_cast<char>(0xe0 | (cp >> 12)));
                separator.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3f)));
                separator.push_back(static_cast<char>(0x80 | (cp & 0x3f)));
            } else {
                separator.push_back(static_cast<char>(0xf0 | (cp >> 18)));
                separator.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3f)));
                separator.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3f)));
                separator.push_back(static_cast<char>(0x80 | (cp & 0x3f)));
            }
        } else {
            throw std::runtime_error("g_string-split separator must be a "
                                     "string or character");
        }
        std::vector<std::string> parts;
        if (separator.empty()) {
            std::size_t index = 0;
            while (index < text.size()) {
                std::size_t width = 0;
                utf8_character_at(text, index, width);
                parts.push_back(text.substr(index, width));
                index += width;
            }
        } else {
            std::size_t start = 0;
            while (true) {
                const std::size_t found = text.find(separator, start);
                if (found == std::string::npos) {
                    parts.push_back(text.substr(start));
                    break;
                }
                parts.push_back(text.substr(start, found - start));
                start = found + separator.size();
            }
        }
        Value result = Value::null();
        for (auto it = parts.rbegin(); it != parts.rend(); ++it)
            result = evaluator.pair(evaluator.string(*it), result);
        return Values{result};
    });

    // (liii vector) aliases vector-filter onto this primitive (the host
    // provides it from s7_liii_vector.c): keep elements whose predicate
    // result is not #f.
    evaluator.define_machine_primitive(
        "g_vector_filter", PrimitiveObject::Kind::VectorFilter);

    // Character index of the first matching character at or after start.
    install(evaluator, "char-position", [&evaluator](const Values& args) {
        if (args.size() < 2 || args.size() > 3)
            throw std::runtime_error(
                "char-position expects two or three arguments");
        if (!args[1].is_object() ||
            args[1].as_object()->type() != ObjectType::String)
            throw std::runtime_error("char-position expects a string");
        const std::string text = evaluator.string_value(args[1]);
        std::int64_t start = 0;
        if (args.size() == 3) {
            if (!args[2].is_integer())
                throw std::runtime_error("char-position start must be an integer");
            start = args[2].as_integer();
            if (start < 0)
                throw std::runtime_error("char-position start must be non-negative");
        }
        if (static_cast<std::size_t>(start) >= utf8_length(text))
            return Values{Value::boolean(false)};
        std::string needles;
        if (args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::Character) {
            needles = utf8_encode_char(args[0].as_object<CharacterObject>()->value);
        } else if (args[0].is_object() &&
                   args[0].as_object()->type() == ObjectType::String) {
            needles = evaluator.string_value(args[0]);
            if (needles.empty()) return Values{Value::boolean(false)};
        } else {
            throw std::runtime_error(
                "char-position expects a character or a string");
        }
        std::vector<char32_t> wanted;
        for (std::size_t position = 0; position < needles.size();) {
            std::size_t width = 0;
            wanted.push_back(utf8_character_at(needles, position, width));
            position += width;
        }
        auto position = utf8_byte_offset(text, start);
        for (auto index = start; position < text.size(); ++index) {
            std::size_t width = 0;
            const auto cp = utf8_character_at(text, position, width);
            if (std::find(wanted.begin(), wanted.end(), cp) != wanted.end())
                return Values{Value::integer(index)};
            position += width;
        }
        return Values{Value::boolean(false)};
    });

    // Identity hashes remain stable across mutations in graph memo tables.
    install(evaluator, "g-identity-hash", [](const Values& args) {
        require_arity(args, 1, "g-identity-hash");
        if (!args[0].is_object())
            throw std::runtime_error("wrong-type-arg: g-identity-hash expects an object");
        const auto address = reinterpret_cast<std::uintptr_t>(args[0].as_object());
        return Values{Value::integer(static_cast<std::int64_t>((address >> 3) & 0x3fffffffffffffff))};
    });

    // s7's hash-code: the srfi-128 default comparators hang off it.  The
    // optional second argument (an eqfunc on the host) is accepted and
    // ignored -- the hash is equal?-consistent, which is what the
    // comparators need for their bucket tables.
    install(evaluator, "hash-code", [](const Values& args) {
        if (args.empty() || args.size() > 2)
            throw std::runtime_error("hash-code expects 1 or 2 arguments");
        if (args[0].is_integer()) {
            constexpr std::uint64_t offset = 1469598103934665603ull;
            constexpr std::uint64_t prime = 1099511628211ull;
            const auto hash =
                (offset ^ static_cast<std::uint64_t>(args[0].as_integer())) * prime;
            return Values{Value::integer(
                static_cast<std::int64_t>(hash & 0x3fffffffffffffff))};
        }
        std::function<std::uint64_t(Value, int)> mix =
            [&mix](Value value, int depth) -> std::uint64_t {
            const std::uint64_t offset = 1469598103934665603ull;
            const std::uint64_t prime = 1099511628211ull;
            auto fold = [&](std::uint64_t seed, std::uint64_t piece) {
                return (seed ^ piece) * prime;
            };
            if (value.is_integer())
                return fold(offset, static_cast<std::uint64_t>(value.as_integer()));
            if (value.is_boolean())
                return fold(offset, value.as_boolean() ? 1 : 0);
            if (value.is_null())
                return offset;
            if (!value.is_object())
                return offset;
            if (depth <= 0)
                return offset;
            switch (value.as_object()->type()) {
                case ObjectType::Character:
                    return fold(offset, value.as_object<CharacterObject>()->value);
                case ObjectType::Symbol: {
                    std::uint64_t hash = offset;
                    for (char byte : value.as_object<SymbolObject>()->name)
                        hash = (hash ^ static_cast<unsigned char>(byte)) * prime;
                    return hash;
                }
                case ObjectType::String: {
                    std::uint64_t hash = offset;
                    for (char byte : value.as_object<StringObject>()->value)
                        hash = (hash ^ static_cast<unsigned char>(byte)) * prime;
                    return hash;
                }
                case ObjectType::Pair: {
                    std::uint64_t hash = offset;
                    Value rest = value;
                    while (rest.is_object() &&
                           rest.as_object()->type() == ObjectType::Pair &&
                           depth > 0) {
                        auto* pair = rest.as_object<PairObject>();
                        hash = fold(hash, mix(pair->car, depth - 1));
                        rest = pair->cdr;
                        --depth;
                    }
                    if (!rest.is_null())
                        hash = fold(hash, mix(rest, depth));
                    return hash;
                }
                case ObjectType::Vector: {
                    std::uint64_t hash = offset;
                    for (Value element : value.as_object<VectorObject>()->values)
                        hash = fold(hash, mix(element, depth - 1));
                    return hash;
                }
                default:
                    return offset;
            }
        };
        return Values{Value::integer(
            static_cast<std::int64_t>(mix(args[0], 8) & 0x3fffffffffffffff))};
    });

    // (string ch ...) : the R7RS character-vector constructor; (scheme base)
    // exports it as a primitive-table name, so native must provide it.
    install(evaluator, "string", [&evaluator](const Values& args) {
        std::string out;
        for (Value arg : args) {
            if (!arg.is_object() ||
                arg.as_object()->type() != ObjectType::Character)
                raise_keyed(evaluator, "wrong-type-arg",
                            "string expects characters");
            const char32_t codepoint =
                arg.as_object<CharacterObject>()->value;
            out += utf8_encode_char(codepoint);
        }
        return Values{evaluator.string(out)};
    });

    // Exact integer floor/truncate division families used by R7RS and SRFI-19.
    auto install_division_family = [&evaluator](
                                       const char* quotient_name,
                                       const char* remainder_name,
                                       const char* both_name,
                                       bool floor_semantics) {
        auto compute = [floor_semantics](BigInteger a, BigInteger b)
            -> std::pair<BigInteger, BigInteger> {
            if (b.is_zero()) throw std::runtime_error("division by zero");
            BigInteger q = a / b;
            BigInteger r = a % b;
            if (floor_semantics && !r.is_zero() &&
                (a.negative() != b.negative())) {
                q -= BigInteger(1);
                r += b;
            }
            return {q, r};
        };
        install(evaluator, both_name, [&evaluator, compute, both_name](const Values& args) {
            require_arity(args, 2, both_name);
            BigInteger a, b;
            if (!integer_value(args[0], a) || !integer_value(args[1], b))
                raise_keyed(evaluator, "wrong-type-arg",
                            std::string(both_name) + " expects integers");
            const bool inexact = number_value(args[0]).real.inexact ||
                                 number_value(args[1]).real.inexact;
            auto qr = compute(std::move(a), std::move(b));
            return Values{make_integer(evaluator, std::move(qr.first), inexact),
                          make_integer(evaluator, std::move(qr.second), inexact)};
        });
        install(evaluator, quotient_name,
                [&evaluator, compute, quotient_name](const Values& args) {
                    require_arity(args, 2, quotient_name);
                    BigInteger a, b;
                    if (!integer_value(args[0], a) || !integer_value(args[1], b))
                        raise_keyed(evaluator, "wrong-type-arg",
                                    std::string(quotient_name) + " expects integers");
                    const bool inexact = number_value(args[0]).real.inexact ||
                                         number_value(args[1]).real.inexact;
                    return Values{make_integer(evaluator,
                        compute(std::move(a), std::move(b)).first, inexact)};
                });
                install(evaluator, remainder_name,
                [&evaluator, compute, remainder_name](const Values& args) {
                    require_arity(args, 2, remainder_name);
                    BigInteger a, b;
                    if (!integer_value(args[0], a) || !integer_value(args[1], b))
                        raise_keyed(evaluator, "type-error",
                                    std::string(remainder_name) + " expects integers");
                    const bool inexact = number_value(args[0]).real.inexact ||
                                         number_value(args[1]).real.inexact;
                    return Values{make_integer(evaluator,
                        compute(std::move(a), std::move(b)).second, inexact)};
                });
    };
    install_division_family("floor-quotient", "floor-remainder", "floor/", true);
    install_division_family("truncate-quotient", "truncate-remainder",
                            "truncate/", false);

    // --- multi-value protocol as first-class procedures.  The core forms
    // --- handle (values ...) and (call-with-values ...) at call sites;
    // --- value positions (procedure?, passing to a combinator, the expander's
    // --- let-values runtime path) need these bindings, like the host has.
    install(evaluator, "values", [](const Values& args) { return args; });
    evaluator.define_machine_primitive(
        "call-with-values", PrimitiveObject::Kind::CallWithValues);

    // --- current ports and dynamic rebinding (R7RS file I/O) --------------
    install(evaluator, "current-input-port", [&evaluator](const Values& args) {
        current_input_port(evaluator);
        return port_parameter(g_current_ports.input, args,
                              ObjectType::InputPort, "current-input-port");
    });
    install(evaluator, "current-error-port", [](const Values& args) {
        return port_parameter(g_current_ports.error_port, args,
                              ObjectType::OutputPort, "current-error-port");
    });
    evaluator.define_machine_primitive(
        "with-input-from-file", PrimitiveObject::Kind::WithInputFromFile);
    evaluator.define_machine_primitive(
        "with-output-to-file", PrimitiveObject::Kind::WithOutputToFile);
    install(evaluator, "read-line", [&evaluator, eof](const Values& args) {
        if (args.size() > 2)
            throw std::runtime_error("read-line expects zero to two arguments");
        Value port_value =
            args.empty() ? current_input_port(evaluator) : args[0];
        const bool with_eol = args.size() == 2 && args[1].is_boolean() &&
                              args[1].as_boolean();
        auto& port = input_port(port_value, "read-line");
        if (port.position >= port.source.size()) return Values{eof};
        const std::size_t newline = port.source.find('\n', port.position);
        std::string line;
        if (newline == std::string::npos) {
            line = port.source.substr(port.position);
            port.position = port.source.size();
        } else {
            line = port.source.substr(port.position, newline - port.position);
            port.position = newline + 1;
        }
        if (!line.empty() && line.back() == '\r') line.pop_back();
        if (with_eol && newline != std::string::npos) line.push_back('\n');
        return Values{evaluator.string(line)};
    });
    install(evaluator, "read-string", [&evaluator, eof](const Values& args) {
        if (args.size() < 1 || args.size() > 2)
            throw std::runtime_error(
                "read-string expects one or two arguments");
        if (!args[0].is_integer())
            throw std::runtime_error("read-string count must be an integer");
        const std::int64_t count = args[0].as_integer();
        if (count < 0)
            throw std::runtime_error("read-string count must be non-negative");
        Value port_value = args.size() == 2 ? args[1] : current_input_port(evaluator);
        auto& port = input_port(port_value, "read-string");
        if (count == 0) return Values{evaluator.string("")};
        if (port.position >= port.source.size()) return Values{eof};
        auto end = port.position;
        for (std::int64_t i = 0; i < count && end < port.source.size(); ++i) {
            std::size_t width = 0;
            utf8_character_at(port.source, end, width);
            end += width;
        }
        const std::string text = port.source.substr(port.position, end - port.position);
        port.position = end;
        return Values{evaluator.string(text)};
    });
    install(evaluator, "char-ready?", [&evaluator](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("char-ready? expects zero or one argument");
        if (args.size() == 1)
            (void)input_port(args[0], "char-ready?"); // validate the port
        return Values{Value::boolean(true)}; // string-backed ports always are
    });
    install(evaluator, "string-position", [&evaluator](const Values& args) {
        if (args.size() < 2 || args.size() > 3)
            throw std::runtime_error(
                "string-position expects two or three arguments");
        const std::string needle = evaluator.string_value(args[0]);
        const std::string text = evaluator.string_value(args[1]);
        std::size_t start = 0;
        if (args.size() == 3) {
            if (!args[2].is_integer())
                throw std::runtime_error(
                    "string-position start must be an integer");
            if (args[2].as_integer() < 0)
                throw std::runtime_error(
                    "string-position start must be non-negative");
            start = static_cast<std::size_t>(args[2].as_integer());
        }
        if (start > utf8_length(text) || needle.empty())
            return Values{Value::boolean(false)};
        const std::size_t at = text.find(needle, utf8_byte_offset(text, start));
        if (at == std::string::npos)
            return Values{Value::boolean(false)};
        return Values{Value::integer(static_cast<std::int64_t>(utf8_length(text.substr(0, at))))};
    });
    install(evaluator, "object->string", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 3)
            throw std::runtime_error(
                "object->string expects one to three arguments");
        // Default is write style (a 1-arg call quotes strings); a false
        // second argument switches to display style.
        const bool write_mode =
            args.size() < 2 || !args[1].is_boolean() || args[1].as_boolean();
        std::string text = format_value(evaluator, args[0], write_mode, 0);
        if (args.size() == 3) {
            if (!args[2].is_integer())
                throw std::runtime_error(
                    "object->string max-len must be an integer");
            const std::int64_t max_len = args[2].as_integer();
            if (max_len >= 0 &&
                static_cast<std::int64_t>(utf8_length(text)) > max_len)
                return Values{evaluator.string(
                    text.substr(0, utf8_byte_offset(text, max_len)) + "...")};
        }
        return Values{evaluator.string(text)};
    });
    evaluator.global_environment()->define(
        evaluator.symbol("pi"),
        evaluator.number(Number::inexact(std::acos(-1.0))));
}

} // namespace goldfish::runtime
