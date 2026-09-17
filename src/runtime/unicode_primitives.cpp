#include "runtime/unicode_primitives.hpp"

#include "runtime/unicode_char.hpp"

#include <stdexcept>

namespace goldfish::runtime {

namespace {

void require_arity(const Values& args, std::size_t count, const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

void install(Evaluator& evaluator, const char* name,
             PrimitiveObject::Function function) {
    evaluator.define_primitive(name, std::move(function));
}

} // namespace

void install_unicode_primitives(Evaluator& evaluator) {
    install(evaluator, "g_char-upcase", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-upcase");
        return Values{evaluator.character(unicode_char_upcase(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
    install(evaluator, "g_char-downcase", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-downcase");
        return Values{evaluator.character(unicode_char_downcase(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
    install(evaluator, "g_char-alphabetic?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-alphabetic?");
        return Values{Value::boolean(unicode_char_alphabetic(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
    install(evaluator, "g_char-upper-case?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-upper-case?");
        return Values{Value::boolean(unicode_char_upper_case(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
    install(evaluator, "g_char-lower-case?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-lower-case?");
        return Values{Value::boolean(unicode_char_lower_case(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
    install(evaluator, "g_char-numeric?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-numeric?");
        return Values{Value::boolean(unicode_char_numeric(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
    install(evaluator, "g_char-whitespace?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-whitespace?");
        return Values{Value::boolean(unicode_char_whitespace(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
    install(evaluator, "g_char-foldcase", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_char-foldcase");
        return Values{evaluator.character(unicode_char_foldcase(
            static_cast<std::uint32_t>(evaluator.character_value(args[0]))))};
    });
}

} // namespace goldfish::runtime
