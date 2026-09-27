#include "runtime/standard_primitives.hpp"

#include "runtime/debug_flags.hpp"
#include "runtime/reader.hpp"
#include "runtime/platform_primitives.hpp"
#include "runtime/unicode_primitives.hpp"

#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstring>
#include <functional>
#include <cstdlib>
#include <fstream>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <limits>
#include <numeric>
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
                message += "<value>";
        }
        return message;
    }
    return "raised Scheme value";
}

std::vector<Value> proper_list(Value value) {
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

bool equal(Value left, Value right) {
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
    const unsigned char first =
        static_cast<unsigned char>(text[position]);
    std::size_t width = 0;
    std::uint32_t codepoint = 0;
    if (first <= 0x7f) {
        width = 1;
        codepoint = first;
    } else if (first >= 0xc2 && first <= 0xdf) {
        width = 2;
        codepoint = first & 0x1f;
    } else if (first >= 0xe0 && first <= 0xef) {
        width = 3;
        codepoint = first & 0x0f;
    } else if (first >= 0xf0 && first <= 0xf4) {
        width = 4;
        codepoint = first & 0x07;
    } else {
        return false;
    }
    if (width > text.size() - position) return false;
    for (std::size_t i = 1; i < width; ++i) {
        const unsigned char continuation =
            static_cast<unsigned char>(text[position + i]);
        if ((continuation & 0xc0) != 0x80) return false;
        codepoint = (codepoint << 6) | (continuation & 0x3f);
    }
    if ((width == 3 && codepoint < 0x800) ||
        (width == 4 && codepoint < 0x10000) ||
        (codepoint >= 0xd800 && codepoint <= 0xdfff) ||
        codepoint > 0x10ffff)
        return false;
    decoded_width = width;
    return true;
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

// write_mode: strings quoted and characters in #\ notation (also for nested
// elements of a displayed structure).  Top-level display prints strings and
// characters raw, matching the host.
std::string format_value(const Evaluator& evaluator, Value value,
                         bool write_mode, int depth) {
    if (depth > 200) return "#<deep>";
    if (value.is_unspecified()) return "#<unspecified>";
    if (value.is_integer()) return std::to_string(value.as_integer());
    if (value.is_boolean()) return value.as_boolean() ? "#t" : "#f";
    if (value.is_null()) return "()";
    if (!value.is_object()) return "#<object>";
    switch (value.as_object()->type()) {
        case ObjectType::String: {
            const std::string& text = value.as_object<StringObject>()->value;
            return write_mode ? "\"" + text + "\"" : text;
        }
        case ObjectType::Symbol:
            return value.as_object<SymbolObject>()->name;
        case ObjectType::Character: {
            const char32_t codepoint = value.as_object<CharacterObject>()->value;
            if (write_mode) return character_literal(codepoint);
            if (codepoint >= 0x20 && codepoint != 0x7f)
                return utf8_encode_char(codepoint);
            return character_literal(codepoint);
        }
        case ObjectType::Eof:
            return "#<eof>";
        case ObjectType::ErrorObject:
            return "#<error " + value.as_object<ErrorObject>()->message + ">";
        case ObjectType::Closure:
        case ObjectType::Primitive:
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
                    sequence.as_object<StringObject>()->value.size());
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
        dest.as_object<StringObject>()->value.replace(
            0, static_cast<std::size_t>(count),
            src.as_object<StringObject>()->value,
            static_cast<std::size_t>(start), static_cast<std::size_t>(count));
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
        for (std::int64_t i = 0; i < count; ++i)
            target[static_cast<std::size_t>(i)] =
                evaluator.character(static_cast<char32_t>(
                    static_cast<unsigned char>(text[static_cast<std::size_t>(start + i)])));
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
                text.push_back(static_cast<char>(
                    element.as_object<CharacterObject>()->value & 0xff));
            else if (element.is_integer())
                text.push_back(static_cast<char>(element.as_integer() & 0xff));
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

void install_runtime_primitives(Evaluator& evaluator) {
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
        auto end = static_cast<std::int64_t>(value.size());
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
            static_cast<std::size_t>(end) > value.size())
            throw std::runtime_error(
                "out-of-range: write-string index out of bounds");
        *output_port(port, "write-string").stream << value.substr(
            static_cast<std::size_t>(start),
            static_cast<std::size_t>(end - start));
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
    // Integer atoms used by the kernel and by the bootstrap libraries.
    install(evaluator, "lognot", [](const Values& args) {
        require_arity(args, 1, "lognot");
        if (!args[0].is_integer())
            throw std::runtime_error("lognot expects an integer");
        return Values{Value::integer(~args[0].as_integer())};
    });
    for (const char* name : {"logand", "logior", "logxor"}) {
        install(evaluator, name, [name](const Values& args) {
            if (args.empty())
                throw std::runtime_error(std::string(name) +
                                         " expects at least one argument");
            for (Value value : args)
                if (!value.is_integer())
                    throw std::runtime_error(std::string(name) +
                                             " expects integers");
            std::int64_t result = args[0].as_integer();
            for (std::size_t i = 1; i < args.size(); ++i) {
                const std::int64_t value = args[i].as_integer();
                if (std::string(name) == "logand") result &= value;
                else if (std::string(name) == "logior") result |= value;
                else result ^= value;
            }
            return Values{Value::integer(result)};
        });
    }
    install(evaluator, "ash", [](const Values& args) {
        require_arity(args, 2, "ash");
        if (!args[0].is_integer() || !args[1].is_integer())
            throw std::runtime_error("ash expects integers");
        const std::int64_t value = args[0].as_integer();
        const std::int64_t shift = args[1].as_integer();
        if (shift >= 0) {
            if (shift >= 63) return Values{Value::integer(0)};
            return Values{Value::integer(static_cast<std::int64_t>(
                static_cast<std::uint64_t>(value) << shift))};
        }
        const std::int64_t amount = shift == std::numeric_limits<std::int64_t>::min()
                                        ? 63 : -shift;
        if (amount >= 63)
            return Values{Value::integer(value < 0 ? -1 : 0)};
        if (value >= 0)
            return Values{Value::integer(value >> amount)};
        const std::uint64_t magnitude =
            static_cast<std::uint64_t>(-(value + 1)) + 1;
        const std::uint64_t rounded =
            (magnitude >> amount) +
            ((magnitude & ((std::uint64_t{1} << amount) - 1)) != 0);
        return Values{Value::integer(-static_cast<std::int64_t>(rounded))};
    });
    install(evaluator, "abs", [](const Values& args) {
        require_arity(args, 1, "abs");
        if (!args[0].is_integer()) throw std::runtime_error("abs expects an integer");
        if (args[0].as_integer() == std::numeric_limits<std::int64_t>::min())
            throw std::runtime_error("abs integer overflow");
        return Values{Value::integer(std::llabs(args[0].as_integer()))};
    });
    for (const char* name : {"min", "max"}) {
        install(evaluator, name, [name](const Values& args) {
            if (args.empty())
                throw std::runtime_error(std::string(name) + " expects an argument");
            // Host-abi parity: non-real arguments raise 'type-error (the
            // classifier maps "expects real numbers"); the old
            // as_integer() require() surfaced the ambiguous
            // "wrong kind" message instead.
            for (const Value& arg : args) {
                if (!arg.is_integer())
                    throw std::runtime_error(std::string(name) +
                                             " expects real numbers");
            }
            std::int64_t result = args[0].as_integer();
            for (std::size_t i = 1; i < args.size(); ++i) {
                std::int64_t value = args[i].as_integer();
                result = std::string(name) == "min" ? std::min(result, value)
                                                     : std::max(result, value);
            }
            return Values{Value::integer(result)};
        });
    }
    install(evaluator, "expt", [](const Values& args) {
        require_arity(args, 2, "expt");
        const double result = std::pow(static_cast<double>(args[0].as_integer()),
                                       static_cast<double>(args[1].as_integer()));
        if (!std::isfinite(result) ||
            result < static_cast<double>(std::numeric_limits<std::int64_t>::min()) ||
            result > static_cast<double>(std::numeric_limits<std::int64_t>::max()))
            throw std::runtime_error("expt result is outside integer range");
        return Values{Value::integer(static_cast<std::int64_t>(result))};
    });
    // Explicit evaluation environments.  Legacy inlet support is installed
    // later by migration_primitives.cpp, not by this runtime layer.
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
            args[0].as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error(
                "eval-environment-define! expects an eval environment");
        args[0].as_object<EvalEnvironmentObject>()->environment->define(
            args[1], args[2]);
        return Values{Value::unspecified()};
    });
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
    install(evaluator, "eval-environment-ref", [](const Values& args) {
        require_arity(args, 2, "eval-environment-ref");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error(
                "eval-environment-ref expects an eval environment");
        return Values{args[0].as_object<EvalEnvironmentObject>()
                          ->environment->lookup(args[1])};
    });
    install(evaluator, "eval", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("eval expects one or two arguments");
        EnvironmentPtr environment = evaluator.global_environment();
        if (args.size() == 2) {
            if (args[1].is_object() &&
                args[1].as_object()->type() == ObjectType::EvalEnvironment)
                environment =
                    args[1].as_object<EvalEnvironmentObject>()->environment;
            else
                throw std::runtime_error(
                    "eval expects an eval environment as its second argument");
        }
        return evaluator.eval_values(args[0], std::move(environment));
    });
    // Private alias for the runtime eval primitive.  (scheme eval) resolves
    // this name instead of `eval', so importing a user-level eval cannot
    // rebind the host evaluator into a recursive loop.
    evaluator.define_primitive("%host-eval", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("eval expects one or two arguments");
        EnvironmentPtr environment = evaluator.global_environment();
        if (args.size() == 2) {
            if (args[1].is_object() &&
                args[1].as_object()->type() == ObjectType::EvalEnvironment)
                environment =
                    args[1].as_object<EvalEnvironmentObject>()->environment;
            else
                throw std::runtime_error(
                    "eval expects an eval environment as its second argument");
        }
        return evaluator.eval_values(args[0], std::move(environment));
    });
    install(evaluator, "dynamic-wind", [&evaluator](const Values& args) {
        require_arity(args, 3, "dynamic-wind");
        evaluator.apply_values(args[0], {});
        try {
            Values result = evaluator.apply_values(args[1], {});
            evaluator.apply_values(args[2], {});
            return result;
        } catch (...) {
            evaluator.apply_values(args[2], {});
            throw;
        }
    });
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
        return Values{Value::object(
            evaluator.heap().make<OutputPortObject>(std::move(stream)))};
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
        require_arity(args, 0, "current-output-port");
        return Values{g_current_ports.output};
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
        while (port->position < port->source.size()) {
            TinyReader reader(evaluator,
                              port->source.substr(port->position));
            std::optional<Value> form = reader.read();
            port->position += reader.position();
            if (!form) break;
            forms.push_back(*form);
        }
        return Values{evaluator.list(forms)};
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
                Values expanded = evaluator.apply_values(
                    expand_library_body,
                    {evaluator.list(syntax_forms), seed_library, context});
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
        return Values{evaluator.character(static_cast<unsigned char>(
            port->source[port->position]))};
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
        return Values{evaluator.character(static_cast<unsigned char>(
            port->source[port->position++]))};
    });
    install(evaluator, "g-delimiter?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g-delimiter?");
        char32_t character = evaluator.character_value(args[0]);
        return Values{Value::boolean(character == U'\0' ||
                                     std::isspace(static_cast<unsigned char>(character)) ||
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
    install(evaluator, "call-with-input-file",
            [&evaluator](const Values& args) {
                require_arity(args, 2, "call-with-input-file");
                Value port = evaluator.apply_values(
                    evaluator.global_environment()->lookup(
                        evaluator.symbol("open-input-file")),
                    {args[0]})[0];
                try {
                    Values result = evaluator.apply_values(args[1], {port});
                    port.as_object<InputStringPortObject>()->closed = true;
                    return result;
                } catch (...) {
                    port.as_object<InputStringPortObject>()->closed = true;
                    throw;
                }
            });
    install(evaluator, "call-with-output-file",
            [&evaluator](const Values& args) {
                require_arity(args, 2, "call-with-output-file");
                Value port = evaluator.apply_values(
                    evaluator.global_environment()->lookup(
                        evaluator.symbol("open-output-file")),
                    {args[0]})[0];
                try {
                    Values result = evaluator.apply_values(args[1], {port});
                    port.as_object<OutputPortObject>()->stream->flush();
                    port.as_object<OutputPortObject>()->closed = true;
                    return result;
                } catch (...) {
                    port.as_object<OutputPortObject>()->closed = true;
                    throw;
                }
            });
    install(evaluator, "get-output-string", [&evaluator](const Values& args) {
        require_arity(args, 1, "get-output-string");
        auto& port = output_port(args[0], "get-output-string");
        auto stream = std::dynamic_pointer_cast<std::ostringstream>(port.stream);
        if (!stream || !port.buffer)
            throw std::runtime_error("get-output-string expects a string port");
        return Values{evaluator.string(stream->str())};
    });
    // String-port conveniences: s7 ships these as builtins, so the kernel's
    // primitive-variables list turns every reference into a bare name that
    // must resolve in the global environment.
    install(evaluator, "with-output-to-string", [&evaluator](const Values& args) {
        require_arity(args, 1, "with-output-to-string");
        Value port = evaluator
                         .apply_values(
                             evaluator.global_environment()->lookup(
                                 evaluator.symbol("open-output-string")),
                             Values{})[0];
        Value saved = g_current_ports.output;
        g_current_ports.output = port;
        try {
            evaluator.apply_values(args[0], {});
        } catch (...) {
            g_current_ports.output = saved;
            throw;
        }
        g_current_ports.output = saved;
        return Values{evaluator.apply_values(
            evaluator.global_environment()->lookup(
                evaluator.symbol("get-output-string")),
            {port})[0]};
    });
    install(evaluator, "call-with-output-string", [&evaluator](const Values& args) {
        require_arity(args, 1, "call-with-output-string");
        Value port = evaluator
                         .apply_values(
                             evaluator.global_environment()->lookup(
                                 evaluator.symbol("open-output-string")),
                             Values{})[0];
        evaluator.apply_values(args[0], {port});
        return Values{evaluator.apply_values(
            evaluator.global_environment()->lookup(
                evaluator.symbol("get-output-string")),
            {port})[0]};
    });
    install(evaluator, "with-input-from-string", [&evaluator](const Values& args) {
        require_arity(args, 2, "with-input-from-string");
        Value port = evaluator.apply_values(
            evaluator.global_environment()->lookup(
                evaluator.symbol("open-input-string")),
            {args[0]})[0];
        Value saved = g_current_ports.input;
        g_current_ports.input = port;
        try {
            Values result = evaluator.apply_values(args[1], {});
            g_current_ports.input = saved;
            return result;
        } catch (...) {
            g_current_ports.input = saved;
            throw;
        }
    });
    install(evaluator, "call-with-input-string", [&evaluator](const Values& args) {
        require_arity(args, 2, "call-with-input-string");
        Value port = evaluator.apply_values(
            evaluator.global_environment()->lookup(
                evaluator.symbol("open-input-string")),
            {args[0]})[0];
        return evaluator.apply_values(args[1], {port});
    });
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
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "number?", [](const Values& args) {
        require_arity(args, 1, "number?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "real?", [](const Values& args) {
        require_arity(args, 1, "real?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    // Exact-integer-only numeric surface: native carries integers only, so
    // the tower's integer specializations are identities/constant answers.
    // The inexact half (exp/log/sin..., inexact->exact) belongs to the
    // float workstream.
    install(evaluator, "exact-integer?", [](const Values& args) {
        require_arity(args, 1, "exact-integer?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "rational?", [](const Values& args) {
        require_arity(args, 1, "rational?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "complex?", [](const Values& args) {
        require_arity(args, 1, "complex?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "exact?", [](const Values& args) {
        require_arity(args, 1, "exact?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "inexact?", [](const Values& args) {
        require_arity(args, 1, "inexact?");
        return Values{Value::boolean(false)};
    });
    install(evaluator, "finite?", [](const Values& args) {
        require_arity(args, 1, "finite?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "infinite?", [](const Values& args) {
        require_arity(args, 1, "infinite?");
        return Values{Value::boolean(false)};
    });
    install(evaluator, "nan?", [](const Values& args) {
        require_arity(args, 1, "nan?");
        return Values{Value::boolean(false)};
    });
    install(evaluator, "exact", [](const Values& args) {
        require_arity(args, 1, "exact");
        if (!args[0].is_integer())
            throw std::runtime_error("exact expects an integer");
        return Values{args[0]};
    });
    install(evaluator, "numerator", [](const Values& args) {
        require_arity(args, 1, "numerator");
        if (!args[0].is_integer())
            throw std::runtime_error("numerator expects an integer");
        return Values{args[0]};
    });
    install(evaluator, "denominator", [](const Values& args) {
        require_arity(args, 1, "denominator");
        if (!args[0].is_integer())
            throw std::runtime_error("denominator expects an integer");
        return Values{Value::integer(1)};
    });
    install(evaluator, "square", [&evaluator](const Values& args) {
        require_arity(args, 1, "square");
        if (!args[0].is_integer())
            throw std::runtime_error("square expects an integer");
        return Values{Value::integer(args[0].as_integer() *
                                     args[0].as_integer())};
    });
    // floor/ceiling/truncate/round are identities on exact integers; the
    // optional digits argument (s7) is accepted and ignored.
    auto install_rounding_identity = [&evaluator](const char* name) {
        install(evaluator, name, [name](const Values& args) -> Values {
            if (args.empty() || args.size() > 2)
                throw std::runtime_error(std::string(name) +
                                         " expects one or two arguments");
            if (!args[0].is_integer())
                throw std::runtime_error(std::string(name) +
                                         " expects an integer (inexact "
                                         "numbers are not supported yet)");
            return Values{args[0]};
        });
    };
    install_rounding_identity("floor");
    install_rounding_identity("ceiling");
    install_rounding_identity("truncate");
    install_rounding_identity("round");
    install(evaluator, "gcd", [](const Values& args) -> Values {
        auto gcd2 = [](std::int64_t a, std::int64_t b) {
            if (a < 0) a = -a;
            if (b < 0) b = -b;
            while (b != 0) {
                std::int64_t t = a % b;
                a = b;
                b = t;
            }
            return a;
        };
        std::int64_t result = 0;
        for (const Value& argument : args) {
            if (!argument.is_integer())
                throw std::runtime_error("gcd expects integers");
            result = gcd2(result, argument.as_integer());
        }
        return Values{Value::integer(result)};
    });
    install(evaluator, "lcm", [](const Values& args) -> Values {
        auto abs_i = [](std::int64_t v) { return v < 0 ? -v : v; };
        auto gcd2 = [](std::int64_t a, std::int64_t b) {
            while (b != 0) {
                std::int64_t t = a % b;
                a = b;
                b = t;
            }
            return a;
        };
        std::int64_t result = 1;
        for (const Value& argument : args) {
            if (!argument.is_integer())
                throw std::runtime_error("lcm expects integers");
            std::int64_t b = abs_i(argument.as_integer());
            if (b == 0) {
                result = 0;
                break;
            }
            // lcm(a, b) = |a * b| / gcd(a, b)
            result = (abs_i(result) / gcd2(abs_i(result), b)) * b;
        }
        return Values{Value::integer(result)};
    });
    install(evaluator, "exact-integer-sqrt", [](const Values& args) -> Values {
        require_arity(args, 1, "exact-integer-sqrt");
        // Split the checks: non-integer -> 'type-error, negative ->
        // 'value-error (the old combined message collapsed both into one
        // classifier bucket).
        if (!args[0].is_integer())
            throw std::runtime_error("exact-integer-sqrt expects integers");
        if (args[0].as_integer() < 0)
            throw std::runtime_error(
                "exact-integer-sqrt n must be non-negative");
        std::uint64_t x = static_cast<std::uint64_t>(args[0].as_integer());
        std::uint64_t bit = 1ull << 62;
        while (bit > x) bit >>= 2;
        std::uint64_t root = 0;
        while (bit != 0) {
            if (x >= root + bit) {
                x -= root + bit;
                root = (root >> 1) + bit;
            } else {
                root >>= 1;
            }
            bit >>= 2;
        }
        return Values{Value::integer(static_cast<std::int64_t>(root)),
                      Value::integer(static_cast<std::int64_t>(
                          args[0].as_integer() -
                          static_cast<std::int64_t>(root * root)))};
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
        Value rest = args[0];
        while (!rest.is_null()) {
            if (!rest.is_object() ||
                rest.as_object()->type() != ObjectType::Pair)
                return Values{Value::boolean(false)};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(true)};
    });
    install(evaluator, "proper-list?", [](const Values& args) {
        require_arity(args, 1, "proper-list?");
        Value rest = args[0];
        while (!rest.is_null()) {
            if (!rest.is_object() ||
                rest.as_object()->type() != ObjectType::Pair)
                return Values{Value::boolean(false)};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(true)};
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
        if (start < 0 || end < start || end > char_count ||
            (args.size() == 2 && char_count > 0 && start == char_count))
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
        std::int64_t length = 0;
        for (unsigned char byte : bytes)
            if ((byte & 0xc0) != 0x80) ++length;
        return Values{Value::integer(length)};
    });
    install(evaluator, "procedure?", [](const Values& args) {
        require_arity(args, 1, "procedure?");
        bool result = args[0].is_object() &&
                      (args[0].as_object()->type() == ObjectType::Closure ||
                       args[0].as_object()->type() == ObjectType::Primitive);
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
        case ObjectType::Pair: return Values{evaluator.symbol("pair")};
        case ObjectType::Symbol: return Values{evaluator.symbol("symbol")};
        case ObjectType::String: return Values{evaluator.symbol("string")};
        case ObjectType::Vector: return Values{evaluator.symbol("vector")};
        case ObjectType::Character: return Values{evaluator.symbol("character")};
        case ObjectType::Primitive:
        case ObjectType::Closure: return Values{evaluator.symbol("procedure")};
        default: return Values{evaluator.symbol("object")};
        }
    });
    install(evaluator, "eq?", [](const Values& args) {
        require_arity(args, 2, "eq?");
        return Values{Value::boolean(same(args[0], args[1]))};
    });
    install(evaluator, "eqv?", [](const Values& args) {
        require_arity(args, 2, "eqv?");
        return Values{Value::boolean(same(args[0], args[1]))};
    });
    install(evaluator, "equal?", [](const Values& args) {
        require_arity(args, 2, "equal?");
        return Values{Value::boolean(equal(args[0], args[1]))};
    });
    install(evaluator, "catch", [&evaluator](const Values& args) {
        require_arity(args, 3, "catch");
        auto matches = [&evaluator](Value wanted, Value actual) {
            return (wanted.is_boolean() && wanted.as_boolean()) ||
                   evaluator.apply_values(
                       evaluator.global_environment()->lookup(
                           evaluator.symbol("equal?")),
                       {wanted, actual})[0].as_boolean();
        };
        // C++ error keys at the catch boundary: message shapes are the
        // runtime's own contract, so classification is a stable table
        // (tests/scheme pins the host's key taxonomy: wrong-type-arg,
        // wrong-number-of-args, out-of-range, division-by-zero,
        // type-error for the s7-builtin-compat string/utf8 ops).
        auto handle_cxx = [&](const std::string& message) -> Values {
            const bool arity =
                message.rfind("wrong number of arguments", 0) == 0 ||
                (message.find("expects") != std::string::npos &&
                 message.find("argument") != std::string::npos);
            const bool oor =
                message.find("out of bounds") != std::string::npos ||
                message.find("out of range") != std::string::npos;
            const bool valerr =
                message.find("non-negative") != std::string::npos;
            const bool div0 =
                message.find("division by zero") != std::string::npos ||
                message.find("divisor is zero") != std::string::npos;
            const bool ioerr =
                message.find("cannot open") != std::string::npos ||
                message.find("cannot delete") != std::string::npos;
            // s7-builtin-compat numeric/string/char ops pin 'type-error, not
            // 'wrong-type-arg (host-abi raised (error 'type-error ...)).
            const bool s7type =
                message.find("string->utf8 expects") != std::string::npos ||
                message.find("utf8->string expects") != std::string::npos ||
                message.find("expects integers") != std::string::npos ||
                message.find("expects real numbers") != std::string::npos ||
                message.find("expected character") != std::string::npos;
            const bool wtype =
                !arity && (message.find("expects") != std::string::npos ||
                           message.find("expected ") != std::string::npos ||
                           message.find("wrong kind") != std::string::npos);
            const char* key = arity   ? "wrong-number-of-args"
                             : oor    ? "out-of-range"
                             : valerr ? "value-error"
                             : div0   ? "division-by-zero"
                             : ioerr  ? "io-error"
                             : s7type ? "type-error"
                             : wtype  ? "wrong-type-arg"
                                      : nullptr;
            if (key == nullptr) {
                if (!matches(args[0], Value::boolean(true))) throw;
                return evaluator.apply_values(
                    args[2],
                    Values{Value::boolean(true),
                           evaluator.pair(evaluator.string(message),
                                          Value::null())});
            }
            const Value key_value = evaluator.symbol(key);
            if (!matches(args[0], key_value)) throw;
            return evaluator.apply_values(
                args[2],
                Values{key_value,
                       evaluator.pair(evaluator.string(message),
                                      Value::null())});
        };
        try {
            return evaluator.apply_values(args[1], {});
        } catch (const ThrownValue& thrown) {
            if (!matches(args[0], thrown.tag())) throw;
            // Host contract: the handler sees (tag info ...) where the
            // payload rides in a LIST -- guard/with-exception-handler take
            // (car info) as the raised object.
            Value payload = Value::null();
            for (auto it = thrown.arguments().rbegin();
                 it != thrown.arguments().rend(); ++it)
                payload = evaluator.pair(*it, payload);
            return evaluator.apply_values(
                args[2], Values{thrown.tag(), payload});
        } catch (const RaisedValue& raised) {
            // raise (a core form) carries a bare payload; s7's raise throws
            // under tag #t, so mirror that and list-wrap the value.  An
            // ErrorObject came from (error key ...): its message holds the
            // key and the irritants are the info list, exactly what the
            // host's catch hands to (lambda (tag info) ...).  The tag to
            // match is therefore the key itself -- checking #t first made
            // (catch 'some-key ...) unable to see keyed raises.
            if (raised.value().is_object() &&
                raised.value().as_object()->type() == ObjectType::ErrorObject) {
                const auto* error =
                    raised.value().as_object<ErrorObject>();
                const bool keyed = !error->key.empty();
                if (!matches(args[0],
                             keyed ? evaluator.symbol(error->key)
                                   : Value::boolean(true)))
                    throw;
                if (keyed) {
                    // (error key ...) : the host hands (key irritants...) --
                    // guard takes (car info) as the first irritant, which is
                    // what the reader's read-error handlers expect.
                    Value payload = Value::null();
                    for (auto it = error->irritants.rbegin();
                         it != error->irritants.rend(); ++it)
                        payload = evaluator.pair(*it, payload);
                    return evaluator.apply_values(
                        args[2], Values{evaluator.symbol(error->key), payload});
                }
                // R7RS (error "text" ...): the raised object is the single
                // info element, so guard binds the error object itself.
                return evaluator.apply_values(
                    args[2],
                    Values{Value::boolean(true),
                           evaluator.pair(raised.value(), Value::null())});
            }
            return evaluator.apply_values(
                args[2],
                Values{Value::boolean(true),
                       evaluator.pair(raised.value(), Value::null())});
        } catch (const std::runtime_error& error) {
            return handle_cxx(error.what());
        } catch (const std::logic_error& error) {
            // value.hpp's require() ("runtime value has the wrong kind").
            return handle_cxx(error.what());
        }
    });
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
    install(evaluator, "vector-length", [&evaluator](const Values& args) {
        require_arity(args, 1, "vector-length");
        return Values{Value::integer(static_cast<std::int64_t>(
            evaluator.vector_values(args[0]).size()))};
    });
    install(evaluator, "vector-ref", [&evaluator](const Values& args) {
        require_arity(args, 2, "vector-ref");
        auto values = evaluator.vector_values(args[0]);
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
        require_arity(args, 1, "number->string");
        return Values{evaluator.string(std::to_string(args[0].as_integer()))};
    });
    install(evaluator, "string->number", [&evaluator](const Values& args) {
        if (args.size() < 1 || args.size() > 2)
            throw std::runtime_error(
                "string->number expects one or two arguments");
        const std::string text = evaluator.string_value(args[0]);
        std::int64_t radix = 10;
        if (args.size() == 2) {
            if (!args[1].is_integer())
                return Values{Value::boolean(false)};
            radix = args[1].as_integer();
            if (radix < 2 || radix > 36) return Values{Value::boolean(false)};
        }
        // Integers only: rationals, decimals and complexes need the numeric
        // types the first-version substrate does not have yet, so they read
        // as #f here (the same as a malformed literal).
        if (text.empty()) return Values{Value::boolean(false)};
        std::size_t index = 0;
        bool negative = false;
        if (text[index] == '+' || text[index] == '-') {
            negative = text[index] == '-';
            ++index;
        }
        if (index >= text.size()) return Values{Value::boolean(false)};
        std::int64_t accumulator = 0;
        for (; index < text.size(); ++index) {
            const char character = text[index];
            int digit = -1;
            if (character >= '0' && character <= '9')
                digit = character - '0';
            else if (character >= 'a' && character <= 'z')
                digit = character - 'a' + 10;
            else if (character >= 'A' && character <= 'Z')
                digit = character - 'A' + 10;
            if (digit < 0 || digit >= radix)
                return Values{Value::boolean(false)};
            if (accumulator > (9223372036854775807LL - digit) / radix)
                return Values{Value::boolean(false)};
            accumulator = accumulator * radix + digit;
        }
        return Values{Value::integer(negative ? -accumulator : accumulator)};
    });
    install(evaluator, "string-length", [&evaluator](const Values& args) {
        require_arity(args, 1, "string-length");
        return Values{Value::integer(static_cast<std::int64_t>(
            evaluator.string_value(args[0]).size()))};
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
            ? static_cast<std::size_t>(args[2].as_integer()) : value.size();
        if (start > end || end > value.size())
            throw std::runtime_error("string-copy index out of bounds");
        return Values{evaluator.string(value.substr(start, end - start))};
    });
    install(evaluator, "string->list", [&evaluator](const Values& args) {
        if (args.empty() || args.size() > 3)
            throw std::runtime_error(
                "string->list expects one to three arguments");
        const std::string& value = evaluator.string_value(args[0]);
        std::int64_t start = 0;
        auto end = static_cast<std::int64_t>(value.size());
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
            static_cast<std::size_t>(end) > value.size())
            throw std::runtime_error(
                "out-of-range: string->list index out of bounds");
        std::vector<Value> chars;
        for (auto i = start; i < end; ++i)
            chars.push_back(evaluator.character(
                static_cast<unsigned char>(value[static_cast<std::size_t>(i)])));
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
        auto& value = args[0].as_object<StringObject>()->value;
        const auto index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= value.size())
            throw std::runtime_error("string-set! index out of bounds");
        value[static_cast<std::size_t>(index)] = static_cast<char>(
            evaluator.character_value(args[2]));
        return Values{Value::unspecified()};
    });
    install(evaluator, "string-copy!", [&evaluator](const Values& args) {
        if (args.size() != 3 && args.size() != 5)
            throw std::runtime_error("string-copy! expects three or five arguments");
        auto& target = args[0].as_object<StringObject>()->value;
        const auto target_start = args[1].as_integer();
        const std::string& source = evaluator.string_value(args[2]);
        const auto source_start = args.size() == 5 ? args[3].as_integer() : 0;
        const auto source_end = args.size() == 5
            ? args[4].as_integer() : static_cast<std::int64_t>(source.size());
        if (target_start < 0 || source_start < 0 || source_end < source_start ||
            source_end > static_cast<std::int64_t>(source.size()) ||
            target_start + source_end - source_start >
                static_cast<std::int64_t>(target.size()))
            throw std::runtime_error("string-copy! index out of bounds");
        target.replace(static_cast<std::size_t>(target_start),
                       static_cast<std::size_t>(source_end - source_start),
                       source, static_cast<std::size_t>(source_start),
                       static_cast<std::size_t>(source_end - source_start));
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
        const char character = static_cast<char>(evaluator.character_value(args[1]));
        std::int64_t start = 0;
        auto end = static_cast<std::int64_t>(value.size());
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
            static_cast<std::size_t>(end) > value.size())
            throw std::runtime_error(
                "out-of-range: string-fill! index out of bounds");
        std::fill(value.begin() + start, value.begin() + end, character);
        return Values{Value::unspecified()};
    });
    install(evaluator, "string-for-each", [&evaluator](const Values& args) {
        if (args.size() < 2)
            throw std::runtime_error("string-for-each expects a procedure and strings");
        std::vector<std::string> strings;
        for (std::size_t i = 1; i < args.size(); ++i)
            strings.push_back(evaluator.string_value(args[i]));
        std::size_t length = strings[0].size();
        for (const auto& string : strings) length = std::min(length, string.size());
        for (std::size_t i = 0; i < length; ++i) {
            Values call;
            for (const auto& string : strings) call.push_back(
                evaluator.character(static_cast<unsigned char>(string[i])));
            evaluator.apply_values(args[0], call);
        }
        return Values{Value::unspecified()};
    });
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
        if (index < 0 || static_cast<std::size_t>(index) >= value.size())
            throw std::runtime_error("string-ref index out of bounds");
        return Values{evaluator.character(
            static_cast<unsigned char>(value[static_cast<std::size_t>(index)]))};
    });
    install(evaluator, "substring", [&evaluator](const Values& args) {
        // The host's substring is (str start [end]) -- s7 accepts the
        // two-argument form as "start to the end of the string", which
        // goldtest's suffix matching (and srfi-13, define-star) rely on.
        if (args.size() < 2 || args.size() > 3)
            throw std::runtime_error(
                "substring expects two or three arguments");
        const std::string& value = evaluator.string_value(args[0]);
        if (!args[1].is_integer())
            throw std::runtime_error(
                "wrong-type-arg: substring start must be an integer");
        auto start = args[1].as_integer();
        auto end = static_cast<std::int64_t>(value.size());
        if (args.size() == 3) {
            if (!args[2].is_integer())
                throw std::runtime_error(
                    "wrong-type-arg: substring end must be an integer");
            end = args[2].as_integer();
        }
        if (start < 0 || end < start ||
            static_cast<std::size_t>(end) > value.size())
            throw std::runtime_error(
                "out-of-range: substring index out of bounds");
        return Values{evaluator.string(value.substr(
            static_cast<std::size_t>(start),
            static_cast<std::size_t>(end - start)))};
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
    install(evaluator, "+", [](const Values& args) {
        std::int64_t result = 0;
        for (Value arg : args) result += arg.as_integer();
        return Values{Value::integer(result)};
    });
    install(evaluator, "-", [](const Values& args) {
        if (args.empty()) throw std::runtime_error("- expects arguments");
        std::int64_t result = args[0].as_integer();
        if (args.size() == 1) result = -result;
        for (std::size_t i = 1; i < args.size(); ++i) result -= args[i].as_integer();
        return Values{Value::integer(result)};
    });
    install(evaluator, "*", [](const Values& args) {
        std::int64_t result = 1;
        for (Value arg : args) result *= arg.as_integer();
        return Values{Value::integer(result)};
    });
    install(evaluator, "=", [](const Values& args) {
        if (args.size() < 2) throw std::runtime_error("= expects two arguments");
        for (std::size_t i = 1; i < args.size(); ++i)
            if (args[i].as_integer() != args[0].as_integer()) return Values{Value::boolean(false)};
        return Values{Value::boolean(true)};
    });
    install(evaluator, "modulo", [](const Values& args) {
        require_arity(args, 2, "modulo");
        // Key parity: non-integers -> 'type-error, zero divisor ->
        // 'division-by-zero (tests/scheme pins both).
        if (!args[0].is_integer() || !args[1].is_integer())
            throw std::runtime_error("modulo expects integers");
        std::int64_t divisor = args[1].as_integer();
        if (divisor == 0)
            throw std::runtime_error("modulo divisor is zero");
        // R7RS floor semantics: the result takes the divisor's sign, so a
        // truncated remainder with mismatched signs gets adjusted (the
        // negative-dividend case below was already correct; (modulo 10
        // -3) must be -2, not C's 1).
        std::int64_t result = args[0].as_integer() % divisor;
        if (result != 0 && ((result < 0) != (divisor < 0)))
            result += divisor;
        return Values{Value::integer(result)};
    });
    install(evaluator, "positive?", [](const Values& args) {
        require_arity(args, 1, "positive?");
        return Values{Value::boolean(args[0].as_integer() > 0)};
    });
    install(evaluator, "zero?", [](const Values& args) {
        require_arity(args, 1, "zero?");
        return Values{Value::boolean(args[0].as_integer() == 0)};
    });
    install(evaluator, "quotient", [](const Values& args) {
        require_arity(args, 2, "quotient");
        if (args[1].as_integer() == 0) throw std::runtime_error("quotient divisor is zero");
        return Values{Value::integer(args[0].as_integer() / args[1].as_integer())};
    });
    install(evaluator, "remainder", [](const Values& args) {
        require_arity(args, 2, "remainder");
        if (args[1].as_integer() == 0) throw std::runtime_error("remainder divisor is zero");
        return Values{Value::integer(args[0].as_integer() % args[1].as_integer())};
    });
    install(evaluator, "/", [](const Values& args) {
        if (args.empty()) throw std::runtime_error("/ expects arguments");
        std::int64_t result = args[0].as_integer();
        if (args.size() == 1) {
            if (result == 0) throw std::runtime_error("division by zero");
            return Values{Value::integer(1 / result)};
        }
        for (std::size_t i = 1; i < args.size(); ++i) {
            auto divisor = args[i].as_integer();
            if (divisor == 0) throw std::runtime_error("division by zero");
            result /= divisor;
        }
        return Values{Value::integer(result)};
    });
    auto install_comparison = [&evaluator](const char* name,
                                           bool (*compare)(std::int64_t, std::int64_t)) {
        install(evaluator, name, [name, compare](const Values& args) {
            if (args.size() < 2) throw std::runtime_error(std::string(name) + " expects two arguments");
            for (std::size_t i = 1; i < args.size(); ++i)
                if (!compare(args[i - 1].as_integer(), args[i].as_integer()))
                    return Values{Value::boolean(false)};
            return Values{Value::boolean(true)};
        });
    };
    install_comparison("<", [](auto a, auto b) { return a < b; });
    install_comparison(">", [](auto a, auto b) { return a > b; });
    install_comparison("<=", [](auto a, auto b) { return a <= b; });
    install_comparison(">=", [](auto a, auto b) { return a >= b; });

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
                const unsigned char lead = text[index];
                std::size_t width = 1;
                if ((lead & 0xe0) == 0xc0) width = 2;
                else if ((lead & 0xf0) == 0xe0) width = 3;
                else if ((lead & 0xf8) == 0xf0) width = 4;
                if (index + width > text.size()) width = 1;
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
    install(evaluator, "g_vector_filter", [&evaluator](const Values& args) {
        require_arity(args, 2, "g_vector_filter");
        Value source = args[1];
        if (!source.is_object() ||
            source.as_object()->type() != ObjectType::Vector)
            throw std::runtime_error("g_vector_filter expects a vector");
        std::vector<Value> kept;
        for (Value element : source.as_object<VectorObject>()->values) {
            Values outcome = evaluator.apply_values(args[0], {element});
            if (outcome.empty())
                throw std::runtime_error(
                    "g_vector_filter predicate returned no value");
            Value verdict = outcome[0];
            if (!verdict.is_boolean() || verdict.as_boolean())
                kept.push_back(element);
        }
        return Values{evaluator.vector(kept)};
    });

    // s7's char-position: byte index of the first byte of `needle' (a
    // character, or any byte of a string set) at or after `start', else #f.
    // The Scheme reader's number/polar parsing calls it; byte semantics
    // match the host, which truncates characters to bytes the same way.
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
        if (static_cast<std::size_t>(start) >= text.size())
            return Values{Value::boolean(false)};
        std::string needles;
        if (args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::Character) {
            needles += static_cast<char>(
                args[0].as_object<CharacterObject>()->value & 0xff);
        } else if (args[0].is_object() &&
                   args[0].as_object()->type() == ObjectType::String) {
            needles = evaluator.string_value(args[0]);
            if (needles.empty()) return Values{Value::boolean(false)};
        } else {
            throw std::runtime_error(
                "char-position expects a character or a string");
        }
        const char* base = text.data() + start;
        const std::size_t remaining = text.size() - static_cast<std::size_t>(start);
        std::size_t best = std::string::npos;
        for (char needle : needles) {
            const void* found = std::memchr(base, needle, remaining);
            if (!found) continue;
            const std::size_t offset =
                static_cast<const char*>(found) - base;
            if (best == std::string::npos || offset < best) best = offset;
        }
        if (best == std::string::npos)
            return Values{Value::boolean(false)};
        return Values{Value::integer(start + static_cast<std::int64_t>(best))};
    });

    // s7's hash-code: the srfi-128 default comparators hang off it.  The
    // optional second argument (an eqfunc on the host) is accepted and
    // ignored -- the hash is equal?-consistent, which is what the
    // comparators need for their bucket tables.
    install(evaluator, "hash-code", [](const Values& args) {
        if (args.empty() || args.size() > 2)
            throw std::runtime_error("hash-code expects 1 or 2 arguments");
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
            if (codepoint > 0xff)
                raise_keyed(evaluator, "out-of-range",
                            "string character exceeds byte range");
            out.push_back(static_cast<char>(codepoint));
        }
        return Values{evaluator.string(out)};
    });

    // --- exact integer floor/truncate division families (R7RS): srfi-19's
    // --- time code and later numeric code split remainders with these.
    auto install_division_family = [&evaluator](
                                       const char* quotient_name,
                                       const char* remainder_name,
                                       const char* both_name,
                                       bool floor_semantics) {
        auto compute = [floor_semantics](std::int64_t a, std::int64_t b)
            -> std::pair<std::int64_t, std::int64_t> {
            if (b == 0) throw std::runtime_error("division by zero");
            std::int64_t q = a / b;
            std::int64_t r = a % b;
            if (floor_semantics && r != 0 && ((a < 0) != (b < 0))) {
                q -= 1;
                r += b;
            }
            return {q, r};
        };
        install(evaluator, both_name, [compute, both_name](const Values& args) {
            require_arity(args, 2, both_name);
            if (!args[0].is_integer() || !args[1].is_integer())
                throw std::runtime_error(std::string(both_name) +
                                         " expects integers");
            auto qr = compute(args[0].as_integer(), args[1].as_integer());
            return Values{Value::integer(qr.first), Value::integer(qr.second)};
        });
        install(evaluator, quotient_name,
                [compute, quotient_name](const Values& args) {
                    require_arity(args, 2, quotient_name);
                    if (!args[0].is_integer() || !args[1].is_integer())
                        throw std::runtime_error(std::string(quotient_name) +
                                                 " expects integers");
                    return Values{Value::integer(
                        compute(args[0].as_integer(), args[1].as_integer())
                            .first)};
                });
        install(evaluator, remainder_name,
                [compute, remainder_name](const Values& args) {
                    require_arity(args, 2, remainder_name);
                    if (!args[0].is_integer() || !args[1].is_integer())
                        throw std::runtime_error(std::string(remainder_name) +
                                                 " expects integers");
                    return Values{Value::integer(
                        compute(args[0].as_integer(), args[1].as_integer())
                            .second)};
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
    install(evaluator, "call-with-values", [&evaluator](const Values& args) {
        require_arity(args, 2, "call-with-values");
        Values produced = evaluator.apply_values(args[0], {});
        return evaluator.apply_values(args[1], produced);
    });

    // --- current ports and dynamic rebinding (R7RS file I/O) --------------
    install(evaluator, "current-input-port", [&evaluator](const Values& args) {
        require_arity(args, 0, "current-input-port");
        return Values{current_input_port(evaluator)};
    });
    install(evaluator, "current-error-port", [](const Values& args) {
        require_arity(args, 0, "current-error-port");
        return Values{g_current_ports.error_port};
    });
    install(evaluator, "with-input-from-file",
            [&evaluator](const Values& args) {
                require_arity(args, 2, "with-input-from-file");
                if (!args[0].is_object() ||
                    args[0].as_object()->type() != ObjectType::String)
                    raise_keyed(evaluator, "type-error",
                                "with-input-from-file: expected string");
                Value saved = g_current_ports.input;
                Value port = evaluator
                                 .apply_values(
                                     evaluator.global_environment()->lookup(
                                         evaluator.symbol("open-input-file")),
                                     {args[0]})[0];
                g_current_ports.input = port;
                Values result;
                try {
                    result = evaluator.apply_values(args[1], {});
                } catch (...) {
                    g_current_ports.input = saved;
                    port.as_object<InputStringPortObject>()->closed = true;
                    throw;
                }
                g_current_ports.input = saved;
                port.as_object<InputStringPortObject>()->closed = true;
                return result;
            });
    install(evaluator, "with-output-to-file",
            [&evaluator](const Values& args) {
                require_arity(args, 2, "with-output-to-file");
                Value saved = g_current_ports.output;
                Value port = evaluator
                                 .apply_values(
                                     evaluator.global_environment()->lookup(
                                         evaluator.symbol("open-output-file")),
                                     {args[0]})[0];
                g_current_ports.output = port;
                auto close_current = [&]() {
                    g_current_ports.output = saved;
                    if (port.is_object() &&
                        port.as_object()->type() == ObjectType::OutputPort) {
                        auto& out = *port.as_object<OutputPortObject>();
                        if (!out.closed) {
                            out.stream->flush();
                            out.closed = true;
                        }
                    }
                };
                Values result;
                try {
                    result = evaluator.apply_values(args[1], {});
                } catch (...) {
                    close_current();
                    throw;
                }
                close_current();
                return result;
            });
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
        if (port.position >= port.source.size()) return Values{eof};
        const std::size_t available = port.source.size() - port.position;
        const std::size_t take =
            static_cast<std::size_t>(count) < available
                ? static_cast<std::size_t>(count)
                : available;
        const std::string text = port.source.substr(port.position, take);
        port.position += take;
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
        if (start > text.size() || needle.empty())
            return Values{Value::boolean(false)};
        const std::size_t at = text.find(needle, start);
        if (at == std::string::npos)
            return Values{Value::boolean(false)};
        return Values{Value::integer(static_cast<std::int64_t>(at))};
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
                static_cast<std::int64_t>(text.size()) > max_len)
                return Values{evaluator.string(
                    text.substr(0, static_cast<std::size_t>(max_len)) + "...")};
        }
        return Values{evaluator.string(text)};
    });
}

} // namespace goldfish::runtime
