#pragma once

#include "runtime/symbol.hpp"

#include <map>

namespace goldfish::runtime {

enum class CoreForm : std::uint8_t {
    Unknown,
    Quote,
    If,
    Begin,
    Values,
    CallWithValues,
    Raise,
    Error,
    ErrorObjectPredicate,
    ErrorObjectMessage,
    ErrorObjectIrritants,
    Guard,
    Apply,
    Lambda,
    Let,
    Letrec,
    LetrecStar,
    Set,
    Define,
    ModuleRef,
    ModuleSet,
    When,
    Unless,
};

class CoreFormRegistry final {
public:
    explicit CoreFormRegistry(SymbolTable& symbols) : symbols_(symbols) {
        register_form("quote", CoreForm::Quote);
        register_form("if", CoreForm::If);
        register_form("begin", CoreForm::Begin);
        register_form("values", CoreForm::Values);
        register_form("call-with-values", CoreForm::CallWithValues);
        register_form("raise", CoreForm::Raise);
        register_form("error", CoreForm::Error);
        register_form("error-object?", CoreForm::ErrorObjectPredicate);
        register_form("error-object-message", CoreForm::ErrorObjectMessage);
        register_form("error-object-irritants", CoreForm::ErrorObjectIrritants);
        register_form("guard", CoreForm::Guard);
        register_form("apply", CoreForm::Apply);
        register_form("lambda", CoreForm::Lambda);
        register_form("let", CoreForm::Let);
        register_form("letrec", CoreForm::Letrec);
        register_form("letrec*", CoreForm::LetrecStar);
        register_form("set!", CoreForm::Set);
        register_form("define", CoreForm::Define);
        register_form("module-ref", CoreForm::ModuleRef);
        register_form("module-set", CoreForm::ModuleSet);
    }

    CoreForm lookup(Value value) const noexcept {
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::Symbol)
            return CoreForm::Unknown;
        const auto& name = value.as_object<SymbolObject>()->name;
        if (name == "when") return CoreForm::When;
        if (name == "unless") return CoreForm::Unless;
        auto it = forms_.find(value.as_object());
        return it == forms_.end() ? CoreForm::Unknown : it->second;
    }

private:
    void register_form(const char* name, CoreForm form) {
        forms_.emplace(symbols_.intern(name).as_object(), form);
    }

    SymbolTable& symbols_;
    std::map<const Object*, CoreForm> forms_;
};

} // namespace goldfish::runtime
