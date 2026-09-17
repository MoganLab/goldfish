#pragma once

#include "runtime/evaluator.hpp"

#include <optional>
#include <map>
#include <string>

namespace goldfish::runtime {

// Reader for lowered/core artifacts only. This is intentionally not the
// user-facing Scheme reader: it has only the data syntax needed by lowered
// artifacts (including vectors and datum labels).
class TinyReader final {
public:
    TinyReader(Evaluator& evaluator, std::string source)
        : evaluator_(evaluator), source_(std::move(source)) {}

    std::optional<Value> read();
    std::size_t position() const noexcept { return position_; }

private:
    void skip_space();
    Value read_form();
    Value read_list(char closing = ')');
    Value read_vector();
    Value read_character();
    Value read_dispatch();
    Value read_string();
    Value read_atom();
    char peek() const;
    char next();
    bool consume(char expected);
    [[noreturn]] void error(const std::string& message) const;

    Evaluator& evaluator_;
    std::string source_;
    std::size_t position_ = 0;
    std::map<std::size_t, Value> labels_;
};

} // namespace goldfish::runtime
