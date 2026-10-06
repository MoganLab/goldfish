#pragma once

#include "runtime/evaluator.hpp"

#include <optional>
#include <map>
#include <optional>
#include <string>
#include <string_view>

namespace goldfish::runtime {

class Evaluator;

class TinyReader final {
public:
    TinyReader(Evaluator& evaluator, std::string source)
        : evaluator_(evaluator), owned_(std::move(source)), source_(owned_) {}

    // Read from an existing buffer without copying it; the caller must keep
    // `source` alive for the reader's lifetime.  Used by read-forms to walk a
    // whole port instead of re-copying the remaining source per form.
    TinyReader(Evaluator& evaluator, const std::string& source,
               std::size_t start)
        : evaluator_(evaluator), source_(source), position_(start) {}

    std::optional<Value> read();
    std::size_t position() const noexcept { return position_; }

private:
    void skip_space();
    Value read_form();
    Value read_list(char closing = ')');
    Value read_vector();
    Value read_character();
    Value read_dispatch();
    std::string read_escaped_text(char closing);
    Value read_string();
    Value read_quoted_symbol();
    Value read_atom();
    char peek() const;
    char next();
    bool consume(char expected);
    [[noreturn]] void error(const std::string& message) const;

    Evaluator& evaluator_;
    std::string owned_;      // non-empty only for the by-value constructor
    std::string_view source_;
    std::size_t position_ = 0;
    std::map<std::size_t, Value> labels_;
};

} // namespace goldfish::runtime
