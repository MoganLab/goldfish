#include "runtime/s7_bridge.hpp"

#include <string>
#include <vector>
#include <stdexcept>
#include <utility>

namespace goldfish::runtime {

namespace {

class S7Roots final {
public:
    explicit S7Roots(gf::scheme* scheme) : scheme_(scheme) {}
    S7Roots(const S7Roots&) = delete;
    S7Roots& operator=(const S7Roots&) = delete;

    void protect(gf::pointer value) {
        locations_.push_back(gf::gc_protect(scheme_, value));
    }

    ~S7Roots() {
        for (auto it = locations_.rbegin(); it != locations_.rend(); ++it)
            gf::gc_unprotect_at(scheme_, *it);
    }

private:
    gf::scheme* scheme_;
    std::vector<gf::int_> locations_;
};

} // namespace

Value S7Bridge::from_s7(gf::scheme* scheme, gf::pointer value) const {
    if (gf::is_null(scheme, value))
        return Value::null();
    if (gf::is_unspecified(scheme, value))
        return Value::unspecified();
    if (gf::is_boolean(value))
        return Value::boolean(gf::boolean(scheme, value));
    if (gf::is_integer(value))
        return Value::integer(gf::integer(value));
    if (gf::is_symbol(value))
        return runtime_.evaluator().symbol(gf::symbol_name(value));
    if (gf::is_string(value))
        return runtime_.evaluator().string(gf::string(value));
    if (gf::is_character(value))
        return runtime_.evaluator().character(gf::character(value));
    if (gf::is_pair(value))
        return runtime_.evaluator().pair(from_s7(scheme, gf::car(value)),
                                         from_s7(scheme, gf::cdr(value)));
    if (gf::is_vector(value)) {
        std::vector<Value> elements;
        const gf::int_ length = gf::vector_length(value);
        elements.reserve(static_cast<std::size_t>(length));
        for (gf::int_ i = 0; i < length; ++i)
            elements.push_back(from_s7(scheme, gf::vector_ref(scheme, value, i)));
        return runtime_.evaluator().vector(elements);
    }

    throw std::runtime_error("s7 bridge cannot import this value");
}

gf::pointer S7Bridge::to_s7(gf::scheme* scheme, Value value) const {
    if (value.is_null())
        return gf::nil(scheme);
    if (value.is_unspecified())
        return gf::unspecified(scheme);
    if (value.is_boolean())
        return gf::make_boolean(scheme, value.as_boolean());
    if (value.is_integer())
        return gf::make_integer(scheme, value.as_integer());

    if (!value.is_object())
        throw std::runtime_error("s7 bridge cannot export this value");

    Object* object = value.as_object();
    switch (object->type()) {
    case ObjectType::Symbol:
        return gf::make_symbol(scheme,
                               static_cast<SymbolObject*>(object)->name.c_str());
    case ObjectType::String:
        return gf::make_string(scheme,
                               static_cast<StringObject*>(object)->value.c_str());
    case ObjectType::Character:
        return gf::make_character(scheme,
                                  static_cast<CharacterObject*>(object)->value);
    case ObjectType::Eof:
        return gf::eof_object(scheme);
    case ObjectType::Pair: {
        const auto* pair = static_cast<PairObject*>(object);
        S7Roots roots(scheme);
        gf::pointer car = to_s7(scheme, pair->car);
        roots.protect(car);
        gf::pointer cdr = to_s7(scheme, pair->cdr);
        roots.protect(cdr);
        return gf::cons(scheme, car, cdr);
    }
    case ObjectType::Vector: {
        const auto* vector = static_cast<VectorObject*>(object);
        S7Roots roots(scheme);
        gf::pointer result = gf::make_vector(
            scheme, static_cast<gf::int_>(vector->values.size()));
        roots.protect(result);
        for (std::size_t i = 0; i < vector->values.size(); ++i) {
            gf::pointer element = to_s7(scheme, vector->values[i]);
            roots.protect(element);
            gf::vector_set(scheme, result, static_cast<gf::int_>(i), element);
        }
        return result;
    }
    default:
        throw std::runtime_error("s7 bridge cannot export a runtime object");
    }
}

Values S7Bridge::eval_s7(gf::scheme* scheme, gf::pointer expression) const {
    Value imported = from_s7(scheme, expression);
    return runtime_.evaluator().eval_values(imported);
}

gf::pointer S7Bridge::values_to_s7(gf::scheme* scheme,
                                   const Values& values) const {
    S7Roots roots(scheme);
    gf::pointer result = gf::nil(scheme);
    roots.protect(result);
    for (auto it = values.rbegin(); it != values.rend(); ++it) {
        gf::pointer element = to_s7(scheme, *it);
        roots.protect(element);
        result = gf::cons(scheme, element, result);
    }
    return result;
}

} // namespace goldfish::runtime
