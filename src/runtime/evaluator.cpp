#include "runtime/evaluator.hpp"

#include <cstdlib>
#include <fstream>
#include <initializer_list>
#include <iterator>
#include <sstream>
#include <stdexcept>

namespace goldfish::runtime {

void Evaluator::define_primitive(const std::string& name,
                                 PrimitiveObject::Function function) {
    global_->define(symbol(name),
                    Value::object(heap_.make<PrimitiveObject>(
                        std::move(function))));
}

void Evaluator::define_callcc_primitive(const std::string& name) {
    global_->define(
        symbol(name),
        Value::object(heap_.make<PrimitiveObject>(
            [](const Values& arguments) -> Values {
                if (arguments.size() != 1)
                    throw std::runtime_error("call/cc expects one procedure");
                throw std::logic_error(
                    "call/cc must be dispatched by the evaluator");
            },
            PrimitiveObject::Kind::CaptureContinuation)));
}

void Evaluator::define_dynamic_wind_primitive(const std::string& name) {
    global_->define(
        symbol(name),
        Value::object(heap_.make<PrimitiveObject>(
            [](const Values& arguments) -> Values {
                if (arguments.size() != 3)
                    throw std::runtime_error(
                        "dynamic-wind expects before, thunk, and after");
                throw std::logic_error(
                    "dynamic-wind must be dispatched by the evaluator");
            },
            PrimitiveObject::Kind::DynamicWind)));
}

void Evaluator::define_machine_primitive(const std::string& name,
                                         PrimitiveObject::Kind kind) {
    global_->define(
        symbol(name),
        Value::object(heap_.make<PrimitiveObject>(
            [](const Values&) -> Values {
                throw std::logic_error(
                    "machine primitive must be dispatched by the evaluator");
            },
            kind)));
}

// The expander's defs frame: created first by the kernel bootstrap and
// then used as the fallback parent of every later parentless frame.
EnvironmentPtr& defs_root_slot() {
    static EnvironmentPtr root;
    return root;
}

Value Evaluator::make_eval_environment(EnvironmentPtr parent) {
    if (!parent) {
        // Parentless frames fall back to the defs root (first frame wins)
        // so bare gensym references stay reachable, instead of aliasing
        // every "new" environment onto one shared set of bindings -- which
        // made module environments clobber each other (the last module to
        // register `remove' decided what every module-ref saw).
        auto frame = std::make_shared<Environment>(
            defs_root_slot() ? defs_root_slot() : global_);
        if (!defs_root_slot()) defs_root_slot() = frame;
        return Value::object(heap_.make<EvalEnvironmentObject>(frame));
    }
    // A fresh frame that FALLS BACK to the explicit parent.
    return Value::object(heap_.make<EvalEnvironmentObject>(
        std::make_shared<Environment>(std::move(parent))));
}

void Evaluator::set_defs_root(EnvironmentPtr frame) {
    defs_root_slot() = std::move(frame);
}

void Evaluator::collect() {
    heap_.collect([this](Tracer& tracer) {
        global_->trace(tracer);
        // The expander's defs frame is a CHILD of the global chain, so it
        // needs its own root; library gensym definitions live there.
        if (defs_root_slot())
            defs_root_slot()->trace(tracer);
        tracer.mark(pending_procedure_);
        for (const Value& argument : pending_arguments_)
            tracer.mark(argument);
        for (const EvalSnapshot* snapshot : active_evaluations_)
            if (snapshot) snapshot->trace(tracer);
        for (Value* root : permanent_roots())
            if (root) tracer.mark(*root);
    });
}

Value Evaluator::list(std::initializer_list<Value> values) {
    Value result = Value::null();
    for (auto it = values.end(); it != values.begin();) {
        --it;
        result = pair(*it, result);
    }
    return result;
}

Value Evaluator::list(const std::vector<Value>& values) {
    Value result = Value::null();
    for (auto it = values.rbegin(); it != values.rend(); ++it)
        result = pair(*it, result);
    return result;
}

Value Evaluator::list_values(const Values& values) {
    Value result = Value::null();
    for (auto it = values.rbegin(); it != values.rend(); ++it)
        result = pair(*it, result);
    return result;
}

std::string Evaluator::symbol_name(Value value) const {
    if (!value.is_object() || value.as_object()->type() != ObjectType::Symbol)
        throw std::runtime_error("expected symbol");
    return value.as_object<SymbolObject>()->name;
}

std::string Evaluator::string_value(Value value) {
    if (!value.is_object() || value.as_object()->type() != ObjectType::String)
        // Host parity: s7 raises 'wrong-type-arg for type mismatches;
        // the message rides as the first irritant for formatters.
        throw RaisedValue(Value::object(heap_.make<ErrorObject>(
            "expected string", ValueList{string("expected string")},
            "wrong-type-arg")));
    return value.as_object<StringObject>()->value;
}

char32_t Evaluator::character_value(Value value) const {
    if (!value.is_object() ||
        value.as_object()->type() != ObjectType::Character)
        throw std::runtime_error("expected character");
    return value.as_object<CharacterObject>()->value;
}

std::vector<Value> Evaluator::vector_values(Value value) const {
    if (!value.is_object() ||
        value.as_object()->type() != ObjectType::Vector)
        throw std::runtime_error("expected vector");
    return value.as_object<VectorObject>()->values;
}

std::vector<Value> Evaluator::proper_list(Value value) const {
    std::vector<Value> result;
    while (!value.is_null()) {
        if (!value.is_object() || value.as_object()->type() != ObjectType::Pair) {
            trace_throw("proper-list");
            throw std::runtime_error("evaluator: expected proper list");
        }
        PairObject* pair_value = value.as_object<PairObject>();
        result.push_back(pair_value->car);
        value = pair_value->cdr;
    }
    return result;
}

Values Evaluator::eval_sequence(Value expressions,
                                EnvironmentPtr environment) {
    Values result{Value::unspecified()};
    for (Value expression : proper_list(expressions))
        result = eval_values(expression, environment);
    return result;
}

Value Evaluator::eval(Value expression, EnvironmentPtr environment) {
    Values result = eval_values(expression, std::move(environment));
    // Single-value contexts collapse a multi-value result to its FIRST
    // value (s7 parity): srfi-8's receive passes its producer unwrapped as
    // an argument, and (define x (values ...)) keeps the first.
    if (result.empty()) return Value::unspecified();
    return result[0];
}

Values Evaluator::eval_values(Value expression, EnvironmentPtr environment) {
    EvalSnapshot state;
    state.machine_id = next_machine_id_++;
    state.expression = expression;
    state.environment = std::move(environment);
    active_evaluations_.push_back(&state);
    struct PopActive final {
        std::vector<EvalSnapshot*>& active;
        ~PopActive() { active.pop_back(); }
    } pop{active_evaluations_};
    return run_machine(state);
}

namespace {

Value first_or_unspecified(const Values& values) {
    return values.empty() ? Value::unspecified() : values.front();
}

} // namespace

Values Evaluator::run_machine(EvalSnapshot& state) {
    auto evaluate = [&state](Value expression, EnvironmentPtr environment) {
        state.expression = expression;
        state.environment = std::move(environment);
        state.values.clear();
        state.returning = false;
    };
    auto return_values = [&state](Values values) {
        state.values = std::move(values);
        state.returning = true;
    };
    auto sequence = [&](Value forms, EnvironmentPtr environment) {
        if (forms.is_null()) {
            return_values({Value::unspecified()});
            return;
        }
        if (!forms.is_object() || forms.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("evaluator: expected proper list");
        PairObject* first = forms.as_object<PairObject>();
        Value rest = first->cdr;
        if (!rest.is_null()) {
            KontFrame frame;
            frame.kind = KontFrame::Kind::Sequence;
            frame.expression = rest;
            frame.environment = environment;
            state.frames.push_back(std::move(frame));
        }
        evaluate(first->car, std::move(environment));
    };
    std::function<void(Value, const Values&)> invoke;
    std::function<void(KontFrame)> advance_hof;
    auto begin_port_callback = [&](PrimitiveObject::Kind kind,
                                   const Values& arguments) {
        using Kind = PrimitiveObject::Kind;
        const bool input_string = kind == Kind::WithInputFromString ||
                                  kind == Kind::CallWithInputString;
        const bool output_string = kind == Kind::WithOutputToString ||
                                   kind == Kind::CallWithOutputString;
        const bool input_file = kind == Kind::WithInputFromFile ||
                                kind == Kind::CallWithInputFile;
        const bool output_file = kind == Kind::WithOutputToFile ||
                                 kind == Kind::CallWithOutputFile;
        const bool callback_port = kind == Kind::CallWithInputString ||
                                   kind == Kind::CallWithOutputString ||
                                   kind == Kind::CallWithInputFile ||
                                   kind == Kind::CallWithOutputFile;
        const bool returns_output = kind == Kind::WithOutputToString ||
                                    kind == Kind::CallWithOutputString;
        const char* name =
            kind == Kind::WithInputFromString ? "with-input-from-string" :
            kind == Kind::WithOutputToString ? "with-output-to-string" :
            kind == Kind::WithInputFromFile ? "with-input-from-file" :
            kind == Kind::WithOutputToFile ? "with-output-to-file" :
            kind == Kind::CallWithInputString ? "call-with-input-string" :
            kind == Kind::CallWithOutputString ? "call-with-output-string" :
            kind == Kind::CallWithInputFile ? "call-with-input-file" :
                                               "call-with-output-file";
        const std::size_t required =
            kind == Kind::WithOutputToString ||
                    kind == Kind::CallWithOutputString
                ? 1
                : 2;
        if (arguments.size() != required)
            throw std::runtime_error(std::string(name) + " expects " +
                                     std::to_string(required) + " arguments");

        Value port = Value::unspecified();
        Value thunk = Value::unspecified();
        if (input_string || input_file) {
            if (input_file &&
                (!arguments[0].is_object() ||
                 arguments[0].as_object()->type() != ObjectType::String)) {
                const std::string message =
                    kind == Kind::WithInputFromFile
                        ? "with-input-from-file: expected string"
                        : "open-input-file: expected string";
                throw RaisedValue(Value::object(heap_.make<ErrorObject>(
                    message, Values{string(message)}, "type-error")));
            }
            const std::string source = string_value(arguments[0]);
            if (input_string) {
                port = Value::object(
                    heap_.make<InputStringPortObject>(source));
            } else {
                std::ifstream input(source, std::ios::binary);
                if (!input)
                    throw std::runtime_error("cannot open input file: " +
                                             source);
                std::string contents(
                    (std::istreambuf_iterator<char>(input)),
                    std::istreambuf_iterator<char>());
                port = Value::object(heap_.make<InputStringPortObject>(
                    std::move(contents)));
            }
        } else if (output_string) {
            auto buffer = std::make_shared<std::string>();
            auto stream = std::make_shared<std::ostringstream>();
            port = Value::object(heap_.make<OutputPortObject>(
                std::move(stream), std::move(buffer)));
        } else {
            const std::string path = string_value(arguments[0]);
            auto stream = std::make_shared<std::ofstream>(
                path, std::ios::binary | std::ios::trunc);
            if (!*stream)
                throw std::runtime_error("cannot open output file: " + path);
            port = Value::object(heap_.make<OutputPortObject>(stream));
            auto& output = *port.as_object<OutputPortObject>();
            output.file_path = path;
            output.file_backed = true;
        }
        thunk = arguments.back();

        DynamicWinder::PortSlot slot = DynamicWinder::PortSlot::None;
        if (kind == Kind::WithInputFromString ||
            kind == Kind::WithInputFromFile)
            slot = DynamicWinder::PortSlot::Input;
        else if (kind == Kind::WithOutputToString ||
                 kind == Kind::WithOutputToFile)
            slot = DynamicWinder::PortSlot::Output;

        const bool close_port = input_file || output_file;
        const bool needs_winder = slot != DynamicWinder::PortSlot::None ||
                                  close_port;
        KontFrame frame;
        frame.kind = KontFrame::Kind::PortCallback;
        frame.auxiliary = port;
        frame.index = needs_winder ? 1 : 0;
        frame.stage = returns_output;
        if (needs_winder) {
            DynamicWinder winder;
            winder.identity = next_winder_identity_++;
            winder.port_slot = slot;
            winder.close_bound_port = close_port;
            winder.bound_port = port;
            if (slot == DynamicWinder::PortSlot::Input) {
                winder.previous_port = current_input_port_value();
                set_current_input_port_value(port);
            } else if (slot == DynamicWinder::PortSlot::Output) {
                winder.previous_port = current_output_port_value();
                set_current_output_port_value(port);
            }
            state.winders.push_back(std::move(winder));
        }
        state.frames.push_back(std::move(frame));
        if (callback_port)
            invoke(thunk, {port});
        else
            invoke(thunk, {});
    };
    invoke = [&](Value procedure, const Values& arguments) {
        if (!procedure.is_object())
            throw std::runtime_error("attempt to apply non-procedure");
        Object* object = procedure.as_object();
        if (object->type() == ObjectType::Continuation)
            throw ContinuationJump{procedure, arguments};
        if (object->type() != ObjectType::Primitive &&
            object->type() != ObjectType::Closure)
            throw std::runtime_error("attempt to apply non-procedure");
        if (object->type() == ObjectType::Primitive) {
            PrimitiveObject* primitive =
                procedure.as_object<PrimitiveObject>();
            switch (primitive->kind) {
            case PrimitiveObject::Kind::WithInputFromString:
            case PrimitiveObject::Kind::WithOutputToString:
            case PrimitiveObject::Kind::WithInputFromFile:
            case PrimitiveObject::Kind::WithOutputToFile:
            case PrimitiveObject::Kind::CallWithInputString:
            case PrimitiveObject::Kind::CallWithOutputString:
            case PrimitiveObject::Kind::CallWithInputFile:
            case PrimitiveObject::Kind::CallWithOutputFile:
                begin_port_callback(primitive->kind, arguments);
                return;
            case PrimitiveObject::Kind::Catch: {
                if (arguments.size() != 3)
                    throw std::runtime_error("catch expects 3 arguments");
                KontFrame frame;
                frame.kind = KontFrame::Kind::Catch;
                frame.auxiliary = arguments[0];
                frame.expression = arguments[2];
                frame.winders = state.winders;
                state.frames.push_back(std::move(frame));
                invoke(arguments[1], {});
                return;
            }
            default:
                break;
            }
            if (primitive->kind == PrimitiveObject::Kind::Map ||
                primitive->kind == PrimitiveObject::Kind::ForEach) {
                if (arguments.size() < 2)
                    throw std::runtime_error(
                        primitive->kind == PrimitiveObject::Kind::Map
                            ? "map expects procedure and list"
                            : "for-each expects procedure and list");
                KontFrame frame;
                frame.kind = primitive->kind == PrimitiveObject::Kind::Map
                                 ? KontFrame::Kind::MapLoop
                                 : KontFrame::Kind::ForEachLoop;
                frame.auxiliary = arguments[0];
                frame.expressions.assign(arguments.begin() + 1,
                                         arguments.end());
                advance_hof(std::move(frame));
                return;
            }
            if (primitive->kind ==
                PrimitiveObject::Kind::StringForEach) {
                if (arguments.size() < 2)
                    throw std::runtime_error(
                        "string-for-each expects a procedure and strings");
                KontFrame frame;
                frame.kind = KontFrame::Kind::ForEachLoop;
                frame.auxiliary = arguments[0];
                for (std::size_t i = 1; i < arguments.size(); ++i) {
                    std::string text = string_value(arguments[i]);
                    std::vector<Value> characters;
                    characters.reserve(text.size());
                    for (unsigned char byte : text)
                        characters.push_back(
                            character(static_cast<char32_t>(byte)));
                    frame.expressions.push_back(list(characters));
                }
                advance_hof(std::move(frame));
                return;
            }
            if (primitive->kind == PrimitiveObject::Kind::VectorFilter) {
                if (arguments.size() != 2)
                    throw std::runtime_error(
                        "g_vector_filter expects 2 arguments");
                if (!arguments[1].is_object() ||
                    arguments[1].as_object()->type() != ObjectType::Vector)
                    throw std::runtime_error(
                        "g_vector_filter expects a vector");
                KontFrame frame;
                frame.kind = KontFrame::Kind::VectorFilterLoop;
                frame.auxiliary = arguments[0];
                frame.expressions =
                    arguments[1].as_object<VectorObject>()->values;
                advance_hof(std::move(frame));
                return;
            }
            if (primitive->kind ==
                PrimitiveObject::Kind::CallWithValues) {
                if (arguments.size() != 2)
                    throw std::runtime_error(
                        "call-with-values expects producer and consumer");
                KontFrame frame;
                frame.kind = KontFrame::Kind::CallWithValuesProducer;
                frame.auxiliary = arguments[1];
                frame.stage = 1;
                state.frames.push_back(std::move(frame));
                invoke(arguments[0], {});
                return;
            }
            if (primitive->kind == PrimitiveObject::Kind::Fold) {
                if (arguments.size() != 3)
                    throw std::runtime_error("fold expects 3 arguments");
                KontFrame frame;
                frame.kind = KontFrame::Kind::FoldLoop;
                frame.auxiliary = arguments[0];
                frame.values = {arguments[1]};
                frame.expressions = proper_list(arguments[2]);
                advance_hof(std::move(frame));
                return;
            }
            if (primitive->kind == PrimitiveObject::Kind::Filter ||
                primitive->kind == PrimitiveObject::Kind::Any ||
                primitive->kind == PrimitiveObject::Kind::Every) {
                const char* name =
                    primitive->kind == PrimitiveObject::Kind::Filter ? "filter"
                    : primitive->kind == PrimitiveObject::Kind::Any ? "any"
                                                                    : "every";
                if (arguments.size() != 2)
                    throw std::runtime_error(std::string(name) +
                                             " expects 2 arguments");
                KontFrame frame;
                frame.kind = primitive->kind == PrimitiveObject::Kind::Filter
                                 ? KontFrame::Kind::FilterLoop
                             : primitive->kind == PrimitiveObject::Kind::Any
                                 ? KontFrame::Kind::AnyLoop
                                 : KontFrame::Kind::EveryLoop;
                frame.auxiliary = arguments[0];
                frame.expressions = proper_list(arguments[1]);
                if (primitive->kind == PrimitiveObject::Kind::Every)
                    frame.values = {Value::boolean(true)};
                advance_hof(std::move(frame));
                return;
            }
            if (primitive->kind == PrimitiveObject::Kind::Member ||
                primitive->kind == PrimitiveObject::Kind::Assoc) {
                if (arguments.size() != 2 && arguments.size() != 3)
                    throw std::runtime_error(
                        primitive->kind == PrimitiveObject::Kind::Member
                            ? "member expects two or three arguments"
                            : "assoc expects two or three arguments");
                KontFrame frame;
                frame.kind = primitive->kind == PrimitiveObject::Kind::Member
                                 ? KontFrame::Kind::MemberLoop
                                 : KontFrame::Kind::AssocLoop;
                frame.expression = arguments[0];
                frame.auxiliary = arguments.size() == 3
                                      ? arguments[2]
                                      : global_->lookup(symbol("equal?"));
                frame.expressions = {arguments[1]};
                advance_hof(std::move(frame));
                return;
            }
            if (primitive->kind == PrimitiveObject::Kind::DynamicWind) {
                if (arguments.size() != 3)
                    throw std::runtime_error(
                        "dynamic-wind expects before, thunk, and after");
                KontFrame frame;
                frame.kind = KontFrame::Kind::DynamicWind;
                frame.expression = arguments[1];
                frame.values = {arguments[0], arguments[2]};
                frame.environment = state.environment;
                state.frames.push_back(std::move(frame));
                invoke(arguments[0], {});
                return;
            }
            if (primitive->kind ==
                PrimitiveObject::Kind::CaptureContinuation) {
                if (arguments.size() != 1)
                    throw std::runtime_error("call/cc expects one procedure");
                EvalSnapshot captured = state;
                captured.expression = Value::unspecified();
                captured.values = {Value::unspecified()};
                captured.returning = true;
                Value continuation = Value::object(
                    heap_.make<ContinuationObject>(std::move(captured)));
                invoke(arguments[0], {continuation});
                return;
            }
            return_values(primitive->function(arguments));
            return;
        }

        ClosureObject* closure = procedure.as_object<ClosureObject>();
        EnvironmentPtr call_environment =
            std::make_shared<Environment>(closure->environment);
        std::vector<Value> required;
        Value formals = closure->formals;
        Value rest = Value::null();
        while (!formals.is_null()) {
            if (!formals.is_object() ||
                formals.as_object()->type() != ObjectType::Pair) {
                rest = formals;
                break;
            }
            PairObject* formal = formals.as_object<PairObject>();
            required.push_back(formal->car);
            formals = formal->cdr;
        }
        if (arguments.size() < required.size() ||
            (rest.is_null() && arguments.size() != required.size()))
            throw std::runtime_error("wrong number of arguments");
        for (std::size_t i = 0; i < required.size(); ++i)
            call_environment->define(required[i], arguments[i]);
        if (!rest.is_null())
            call_environment->define(
                rest, list_values(Values(arguments.begin() + required.size(),
                                          arguments.end())));
        sequence(closure->body, std::move(call_environment));
    };
    advance_hof = [&](KontFrame frame) {
        if (frame.kind == KontFrame::Kind::VectorFilterLoop) {
            if (frame.index == frame.expressions.size()) {
                return_values({vector(frame.values)});
                return;
            }
            Value element = frame.expressions[frame.index];
            state.frames.push_back(std::move(frame));
            invoke(state.frames.back().auxiliary, {element});
            return;
        }
        if (frame.kind == KontFrame::Kind::FilterLoop ||
            frame.kind == KontFrame::Kind::AnyLoop ||
            frame.kind == KontFrame::Kind::EveryLoop) {
            if (frame.index == frame.expressions.size()) {
                if (frame.kind == KontFrame::Kind::FilterLoop)
                    return_values({list(frame.values)});
                else if (frame.kind == KontFrame::Kind::AnyLoop)
                    return_values({Value::boolean(false)});
                else
                    return_values(std::move(frame.values));
                return;
            }
            Value item = frame.expressions[frame.index];
            state.frames.push_back(std::move(frame));
            invoke(state.frames.back().auxiliary, {item});
            return;
        }
        if (frame.kind == KontFrame::Kind::MemberLoop ||
            frame.kind == KontFrame::Kind::AssocLoop) {
            const bool is_assoc = frame.kind == KontFrame::Kind::AssocLoop;
            while (!frame.expressions.front().is_null()) {
                Value tail = frame.expressions.front();
                if (!tail.is_object() ||
                    tail.as_object()->type() != ObjectType::Pair)
                    throw std::runtime_error(
                        is_assoc ? "assoc expects an association list"
                                 : "member expects a proper list");
                Value entry = tail.as_object<PairObject>()->car;
                if (!is_assoc ||
                    (entry.is_object() &&
                     entry.as_object()->type() == ObjectType::Pair)) {
                    Value key = is_assoc ? entry.as_object<PairObject>()->car
                                         : entry;
                    frame.values = {entry};
                    state.frames.push_back(std::move(frame));
                    invoke(state.frames.back().auxiliary,
                           {state.frames.back().expression, key});
                    return;
                }
                frame.expressions.front() =
                    tail.as_object<PairObject>()->cdr;
            }
            return_values({Value::boolean(false)});
            return;
        }
        if (frame.kind == KontFrame::Kind::FoldLoop) {
            if (frame.index == frame.expressions.size()) {
                return_values(std::move(frame.values));
                return;
            }
            Values call_arguments{frame.values.front(),
                                  frame.expressions[frame.index]};
            state.frames.push_back(std::move(frame));
            invoke(state.frames.back().auxiliary, call_arguments);
            return;
        }

        const bool is_map = frame.kind == KontFrame::Kind::MapLoop;
        std::vector<Value> call_arguments;
        bool all_pairs = true;
        for (Value iterator : frame.expressions) {
            if (!iterator.is_object() ||
                iterator.as_object()->type() != ObjectType::Pair) {
                all_pairs = false;
                break;
            }
        }
        if (is_map && !all_pairs) {
            return_values({list(frame.values)});
            return;
        }
        if (!is_map) {
            if (frame.expressions.front().is_null()) {
                return_values({Value::unspecified()});
                return;
            }
            for (std::size_t i = 1; i < frame.expressions.size(); ++i) {
                Value iterator = frame.expressions[i];
                if (!iterator.is_object() ||
                    iterator.as_object()->type() != ObjectType::Pair) {
                    return_values({Value::unspecified()});
                    return;
                }
            }
            if (!frame.expressions.front().is_object() ||
                frame.expressions.front().as_object()->type() !=
                    ObjectType::Pair)
                throw std::runtime_error("for-each: expected a list");
        }
        for (Value iterator : frame.expressions)
            call_arguments.push_back(iterator.as_object<PairObject>()->car);
        state.frames.push_back(std::move(frame));
        invoke(state.frames.back().auxiliary, call_arguments);
    };
    auto named = [](Value value, const char* name) {
        return value.is_object() &&
               value.as_object()->type() == ObjectType::Symbol &&
               value.as_object<SymbolObject>()->name == name;
    };
    std::function<void(KontFrame)> next_guard_clause;
    next_guard_clause = [&](KontFrame frame) {
        while (!frame.expression.is_null()) {
            PairObject* clause_pair = frame.expression.as_object<PairObject>();
            std::vector<Value> clause = proper_list(clause_pair->car);
            frame.expression = clause_pair->cdr;
            if (clause.empty())
                throw std::runtime_error("empty guard clause");
            EnvironmentPtr handler = frame.secondary_environment;
            if (named(clause[0], "else")) {
                if (clause.size() == 3 && named(clause[1], "=>")) {
                    frame.stage = 2;
                    frame.expressions = {clause[2]};
                    state.frames.push_back(std::move(frame));
                    evaluate(clause[2], std::move(handler));
                } else {
                    sequence(list(std::vector<Value>(clause.begin() + 1,
                                                     clause.end())),
                             std::move(handler));
                }
                return;
            }
            frame.stage = 1;
            frame.index = clause.size() == 3 && named(clause[1], "=>");
            frame.expressions.clear();
            if (frame.index)
                frame.expressions.push_back(clause[2]);
            else
                frame.expressions.assign(clause.begin() + 1, clause.end());
            state.frames.push_back(std::move(frame));
            evaluate(clause[0], std::move(handler));
            return;
        }
        throw RaisedValue(frame.values.front());
    };
    std::function<void(KontFrame)> continue_transfer;
    auto leave_winder = [&](const DynamicWinder& winder) {
        if (winder.close_bound_port && winder.bound_port.is_object()) {
            Object* object = winder.bound_port.as_object();
            if (object->type() == ObjectType::InputPort) {
                winder.bound_port.as_object<InputStringPortObject>()->closed =
                    true;
            } else if (object->type() == ObjectType::OutputPort) {
                auto& port = *winder.bound_port.as_object<OutputPortObject>();
                if (port.stream) port.stream->flush();
                if (port.file_backed && port.stream) {
                    auto file = std::dynamic_pointer_cast<std::ofstream>(
                        port.stream);
                    if (file && file->is_open()) file->close();
                }
                port.closed = true;
            }
        }
        if (winder.port_slot == DynamicWinder::PortSlot::Input)
            set_current_input_port_value(winder.previous_port);
        else if (winder.port_slot == DynamicWinder::PortSlot::Output)
            set_current_output_port_value(winder.previous_port);
    };
    auto enter_winder = [&](const DynamicWinder& winder) {
        if (winder.bound_port.is_object() &&
            winder.bound_port.as_object()->type() == ObjectType::InputPort) {
            winder.bound_port.as_object<InputStringPortObject>()->closed =
                false;
        } else if (winder.bound_port.is_object() &&
                   winder.bound_port.as_object()->type() ==
                       ObjectType::OutputPort) {
            auto& port = *winder.bound_port.as_object<OutputPortObject>();
            if (port.closed && port.file_backed) {
                auto file = std::make_shared<std::ofstream>(
                    port.file_path, std::ios::binary | std::ios::app);
                if (!*file)
                    throw std::runtime_error("cannot reopen output file: " +
                                             port.file_path);
                port.stream = std::move(file);
            }
            port.closed = false;
        }
        if (winder.port_slot == DynamicWinder::PortSlot::Input)
            set_current_input_port_value(winder.bound_port);
        else if (winder.port_slot == DynamicWinder::PortSlot::Output)
            set_current_output_port_value(winder.bound_port);
    };
    continue_transfer = [&](KontFrame frame) {
        auto run_exiting = [&]() -> bool {
            while (frame.index < frame.exiting_winders.size()) {
                const DynamicWinder& winder =
                    frame.exiting_winders[frame.index];
                if (state.winders.empty())
                    throw std::runtime_error("dynamic-wind stack mismatch");
                state.winders.pop_back();
                if (winder.port_slot != DynamicWinder::PortSlot::None ||
                    winder.close_bound_port) {
                    leave_winder(winder);
                    ++frame.index;
                    continue;
                }
                state.frames.push_back(std::move(frame));
                invoke(winder.after, {});
                return false;
            }
            return true;
        };
        auto run_entering = [&]() -> bool {
            while (frame.index < frame.entering_winders.size()) {
                const DynamicWinder& winder =
                    frame.entering_winders[frame.index];
                if (winder.port_slot != DynamicWinder::PortSlot::None ||
                    winder.close_bound_port) {
                    enter_winder(winder);
                    state.winders.push_back(winder);
                    ++frame.index;
                    continue;
                }
                frame.stage = 3;
                state.frames.push_back(std::move(frame));
                invoke(winder.before, {});
                return false;
            }
            return true;
        };
        if (frame.stage == 0) {
            frame.stage = 1;
            frame.index = 0;
            if (!run_exiting()) return;
            frame.stage = 2;
        } else if (frame.stage == 1) {
            ++frame.index;
            if (!run_exiting()) return;
            frame.stage = 2;
        }
        if (frame.stage == 2) {
            frame.index = 0;
            if (!run_entering()) return;
            frame.stage = 4;
        } else if (frame.stage == 3) {
            state.winders.push_back(frame.entering_winders[frame.index]);
            ++frame.index;
            if (!run_entering()) return;
            frame.stage = 4;
        }
        if (!frame.auxiliary.is_object() ||
            frame.auxiliary.as_object()->type() != ObjectType::Continuation)
            throw std::runtime_error("invalid continuation transfer");
        const std::uint64_t running_machine_id = state.machine_id;
        state = frame.auxiliary.as_object<ContinuationObject>()->snapshot;
        // The saved continuation supplies control state, not a new active
        // evaluator invocation. Keep this run's identity so later jumps can
        // still find it in active_evaluations_.
        state.machine_id = running_machine_id;
        state.values = std::move(frame.values);
        state.returning = true;
    };
    auto route_to_guard = [&](std::size_t handler_index, Value caught) {
        KontFrame guard = state.frames[handler_index - 1];
        guard.secondary_environment =
            std::make_shared<Environment>(guard.environment);
        guard.secondary_environment->define(guard.auxiliary, caught);
        guard.values = {caught};
        guard.stage = 3;

        EvalSnapshot target;
        target.machine_id = state.machine_id;
        target.frames.assign(state.frames.begin(),
                             state.frames.begin() + handler_index - 1);
        target.frames.push_back(guard);
        target.environment = guard.environment;
        target.winders = guard.winders;
        target.values = {Value::unspecified()};
        target.returning = true;
        Value target_continuation = Value::object(
            heap_.make<ContinuationObject>(std::move(target)));

        const EvalSnapshot& target_state =
            target_continuation.as_object<ContinuationObject>()->snapshot;
        std::size_t common = 0;
        while (common < state.winders.size() &&
               common < target_state.winders.size() &&
               state.winders[common].identity ==
                   target_state.winders[common].identity)
            ++common;
        KontFrame transfer;
        transfer.kind = KontFrame::Kind::ContinuationTransfer;
        transfer.auxiliary = target_continuation;
        transfer.values = {Value::unspecified()};
        for (std::size_t i = state.winders.size(); i > common; --i)
            transfer.exiting_winders.push_back(state.winders[i - 1]);
        for (std::size_t i = common; i < target_state.winders.size(); ++i)
            transfer.entering_winders.push_back(target_state.winders[i]);

        // Keep the active guard below exit thunks so an exception raised by
        // an after thunk can still be delivered to this guard. The synthetic
        // continuation replaces it once the wind transition completes.
        state.frames.resize(handler_index);
        state.frames.push_back(transfer);
        continue_transfer(std::move(transfer));
    };
    auto unwind_before_rethrow = [&](bool is_raised, Value raised_value,
                                     const std::string& message) {
        EvalSnapshot target;
        target.machine_id = state.machine_id;
        KontFrame rethrow;
        rethrow.kind = is_raised ? KontFrame::Kind::RethrowRaised
                                 : KontFrame::Kind::RethrowRuntimeError;
        if (is_raised)
            rethrow.values = {raised_value};
        else
            rethrow.expression = string(message);
        target.frames.push_back(std::move(rethrow));
        target.values = {Value::unspecified()};
        target.returning = true;
        Value continuation = Value::object(
            heap_.make<ContinuationObject>(std::move(target)));
        KontFrame transfer;
        transfer.kind = KontFrame::Kind::ContinuationTransfer;
        transfer.auxiliary = continuation;
        transfer.values = {Value::unspecified()};
        for (auto it = state.winders.rbegin(); it != state.winders.rend(); ++it)
            transfer.exiting_winders.push_back(*it);
        state.frames.clear();
        continue_transfer(std::move(transfer));
    };
    auto route_to_catch = [&](Value tag, Value info,
                              bool respect_guard_boundary = false) {
        for (std::size_t i = state.frames.size(); i > 0; --i) {
            const KontFrame& candidate = state.frames[i - 1];
            if (respect_guard_boundary &&
                candidate.kind == KontFrame::Kind::ExceptionHandler &&
                candidate.stage == 0)
                return false;
            if (candidate.kind != KontFrame::Kind::Catch) continue;
            if (!(candidate.auxiliary.is_boolean() &&
                  candidate.auxiliary.as_boolean()) &&
                !equal(candidate.auxiliary, tag))
                continue;
            EvalSnapshot target;
            target.machine_id = state.machine_id;
            target.frames.assign(state.frames.begin(),
                                 state.frames.begin() + i - 1);
            target.environment = state.environment;
            target.winders = candidate.winders;
            target.applying = true;
            target.initial_procedure = candidate.expression;
            target.initial_arguments = {tag, info};
            target.values = {Value::unspecified()};
            target.returning = true;
            Value continuation = Value::object(
                heap_.make<ContinuationObject>(std::move(target)));
            const EvalSnapshot& target_state =
                continuation.as_object<ContinuationObject>()->snapshot;
            std::size_t common = 0;
            while (common < state.winders.size() &&
                   common < target_state.winders.size() &&
                   state.winders[common].identity ==
                       target_state.winders[common].identity)
                ++common;
            KontFrame transfer;
            transfer.kind = KontFrame::Kind::ContinuationTransfer;
            transfer.auxiliary = continuation;
            transfer.values = {Value::unspecified()};
            for (std::size_t j = state.winders.size(); j > common; --j)
                transfer.exiting_winders.push_back(state.winders[j - 1]);
            for (std::size_t j = common;
                 j < target_state.winders.size(); ++j)
                transfer.entering_winders.push_back(target_state.winders[j]);
            state.frames.resize(i - 1);
            state.frames.push_back(transfer);
            continue_transfer(std::move(transfer));
            return true;
        }
        return false;
    };
    auto runtime_error_tag = [&](const std::string& message) {
        const bool arity =
            message.rfind("wrong number of arguments", 0) == 0 ||
            (message.find("expects") != std::string::npos &&
             message.find("argument") != std::string::npos);
        const bool oor = message.find("out of bounds") != std::string::npos ||
                         message.find("out of range") != std::string::npos;
        const bool valerr = message.find("non-negative") != std::string::npos;
        const bool div0 = message.find("division by zero") != std::string::npos ||
                          message.find("divisor is zero") != std::string::npos;
        const bool ioerr = message.find("cannot open") != std::string::npos ||
                           message.find("cannot delete") != std::string::npos;
        const bool s7type =
            message.find("string->utf8 expects") != std::string::npos ||
            message.find("utf8->string expects") != std::string::npos ||
            message.find("expects integers") != std::string::npos ||
            message.find("expects real numbers") != std::string::npos ||
            message.find("expected character") != std::string::npos;
        const bool wtype = !arity &&
            (message.find("expects") != std::string::npos ||
             message.find("expected ") != std::string::npos ||
             message.find("wrong kind") != std::string::npos);
        const char* key = arity ? "wrong-number-of-args"
                         : oor ? "out-of-range"
                         : valerr ? "value-error"
                         : div0 ? "division-by-zero"
                         : ioerr ? "io-error"
                         : s7type ? "type-error"
                         : wtype ? "wrong-type-arg" : nullptr;
        return key ? symbol(key) : Value::boolean(true);
    };

restart_machine:
    try {
        if (state.applying) {
            Value procedure = state.initial_procedure;
            Values arguments = std::move(state.initial_arguments);
            state.initial_procedure = Value::unspecified();
            state.applying = false;
            invoke(procedure, arguments);
        }
        while (true) {
            if (state.applying) {
                Value procedure = state.initial_procedure;
                Values arguments = std::move(state.initial_arguments);
                state.initial_procedure = Value::unspecified();
                state.applying = false;
                invoke(procedure, arguments);
            }
            if (!state.returning) {
                Value expression = state.expression;
                EnvironmentPtr environment = state.environment;
                if (!expression.is_object()) {
                    return_values({expression});
                    continue;
                }
                Object* object = expression.as_object();
                if (object->type() == ObjectType::Symbol) {
                    try {
                        return_values({environment->lookup(expression)});
                    } catch (const UnboundSymbolError& error) {
                        const std::string& name =
                            expression.as_object<SymbolObject>()->name;
                        if (!name.empty() &&
                            (name.front() == ':' || name.back() == ':')) {
                            return_values({expression});
                            continue;
                        }
                        throw RaisedValue(Value::object(heap_.make<ErrorObject>(
                            error.what(), ValueList{string(error.what())},
                            "unbound-variable")));
                    }
                    continue;
                }
                if (object->type() != ObjectType::Pair) {
                    return_values({expression});
                    continue;
                }
                PairObject* pair_expression =
                    expression.as_object<PairObject>();
                CoreForm form = core_forms_.lookup(pair_expression->car);
                std::vector<Value> args = proper_list(pair_expression->cdr);
                switch (form) {
                case CoreForm::Quote:
                    if (args.size() != 1)
                        throw std::runtime_error("quote expects one argument");
                    return_values({args[0]});
                    break;
                case CoreForm::Lambda: {
                    if (args.size() < 2)
                        throw std::runtime_error("lambda expects formals and body");
                    Value body = list(std::vector<Value>(args.begin() + 1,
                                                         args.end()));
                    return_values({Value::object(heap_.make<ClosureObject>(
                        args[0], body, environment))});
                    break;
                }
                case CoreForm::If: {
                    if (args.size() != 2 && args.size() != 3)
                        throw std::runtime_error("if expects two or three arguments");
                    KontFrame frame;
                    frame.kind = KontFrame::Kind::IfTest;
                    frame.expression = args[1];
                    frame.auxiliary = args.size() == 3 ? args[2]
                                                       : Value::unspecified();
                    frame.environment = environment;
                    state.frames.push_back(std::move(frame));
                    evaluate(args[0], std::move(environment));
                    break;
                }
                case CoreForm::Begin:
                    sequence(pair_expression->cdr, std::move(environment));
                    break;
                case CoreForm::When:
                case CoreForm::Unless: {
                    if (args.size() < 2)
                        throw std::runtime_error("when/unless expects a test and body");
                    KontFrame frame;
                    frame.kind = KontFrame::Kind::WhenTest;
                    frame.expression = list(std::vector<Value>(args.begin() + 1,
                                                                args.end()));
                    frame.auxiliary = form == CoreForm::When
                                          ? Value::boolean(true)
                                          : Value::boolean(false);
                    frame.environment = environment;
                    state.frames.push_back(std::move(frame));
                    evaluate(args[0], std::move(environment));
                    break;
                }
                case CoreForm::Let:
                case CoreForm::Letrec:
                case CoreForm::LetrecStar: {
                    if (args.size() < 2)
                        throw std::runtime_error("binding form expects bindings and body");
                    std::vector<Value> bindings = proper_list(args[0]);
                    std::vector<Value> init_pairs;
                    init_pairs.reserve(bindings.size() * 2);
                    for (Value binding : bindings) {
                        std::vector<Value> pair_binding = proper_list(binding);
                        if (pair_binding.size() != 2)
                            throw std::runtime_error("binding expects name and value");
                        init_pairs.push_back(pair_binding[0]);
                        init_pairs.push_back(pair_binding[1]);
                    }
                    EnvironmentPtr child = std::make_shared<Environment>(environment);
                    std::size_t stage = form == CoreForm::Let
                                            ? 0
                                            : form == CoreForm::Letrec ? 1 : 2;
                    if (stage != 0) {
                        for (std::size_t i = 0; i < init_pairs.size(); i += 2)
                            child->define(init_pairs[i], Value::object(
                                heap_.make<UninitializedObject>()));
                    }
                    if (init_pairs.empty()) {
                        sequence(list(std::vector<Value>(args.begin() + 1,
                                                         args.end())),
                                  std::move(child));
                        break;
                    }
                    KontFrame frame;
                    frame.kind = stage == 0 ? KontFrame::Kind::LetBinding
                                            : KontFrame::Kind::LetrecBinding;
                    frame.expression = list(std::vector<Value>(args.begin() + 1,
                                                                args.end()));
                    frame.environment = child;
                    frame.secondary_environment = environment;
                    frame.expressions = std::move(init_pairs);
                    frame.stage = stage;
                    state.frames.push_back(std::move(frame));
                    EnvironmentPtr init_environment =
                        stage == 0 ? environment : child;
                    evaluate(state.frames.back().expressions[1],
                             std::move(init_environment));
                    break;
                }
                case CoreForm::Define:
                case CoreForm::Set: {
                    if (args.size() != 2)
                        throw std::runtime_error(form == CoreForm::Define
                                                     ? "define expects name and value"
                                                     : "set! expects name and value");
                    KontFrame frame;
                    frame.kind = form == CoreForm::Define
                                     ? KontFrame::Kind::DefineValue
                                     : KontFrame::Kind::SetValue;
                    frame.expression = args[0];
                    frame.environment = environment;
                    state.frames.push_back(std::move(frame));
                    evaluate(args[1], std::move(environment));
                    break;
                }
                case CoreForm::Apply: {
                    if (args.size() < 2)
                        throw std::runtime_error("apply expects procedure and arguments");
                    KontFrame frame;
                    frame.kind = KontFrame::Kind::ApplyProcedure;
                    frame.expression = list(std::vector<Value>(args.begin() + 1,
                                                                args.end()));
                    frame.environment = environment;
                    state.frames.push_back(std::move(frame));
                    evaluate(args[0], std::move(environment));
                    break;
                }
                case CoreForm::Values: {
                    if (args.empty()) {
                        return_values({});
                        break;
                    }
                    KontFrame frame;
                    frame.kind = KontFrame::Kind::ValuesArgument;
                    frame.expression = list(std::vector<Value>(args.begin() + 1,
                                                                args.end()));
                    frame.environment = environment;
                    state.frames.push_back(std::move(frame));
                    evaluate(args[0], std::move(environment));
                    break;
                }
                case CoreForm::CallWithValues: {
                    if (args.size() != 2)
                        throw std::runtime_error("call-with-values expects producer and consumer");
                    KontFrame frame;
                    frame.kind = KontFrame::Kind::CallWithValuesProducer;
                    frame.expression = args[1];
                    frame.environment = environment;
                    state.frames.push_back(std::move(frame));
                    evaluate(args[0], std::move(environment));
                    break;
                }
                case CoreForm::Guard: {
                    if (args.size() < 2)
                        throw std::runtime_error("guard expects clauses and body");
                    std::vector<Value> specification = proper_list(args[0]);
                    if (specification.empty())
                        throw std::runtime_error("guard expects a binding");
                    KontFrame frame;
                    frame.kind = KontFrame::Kind::ExceptionHandler;
                    frame.expression = list(std::vector<Value>(
                        specification.begin() + 1, specification.end()));
                    frame.auxiliary = specification[0];
                    frame.environment = environment;
                    frame.winders = state.winders;
                    state.frames.push_back(std::move(frame));
                    sequence(list(std::vector<Value>(args.begin() + 1,
                                                     args.end())),
                             std::move(environment));
                    break;
                }
                case CoreForm::Raise:
                    if (args.size() != 1)
                        throw std::runtime_error("raise expects one argument");
                    {
                        KontFrame frame;
                        frame.kind = KontFrame::Kind::RaiseValue;
                        state.frames.push_back(std::move(frame));
                        evaluate(args[0], std::move(environment));
                    }
                    break;
                case CoreForm::Error:
                    if (args.empty())
                        throw std::runtime_error("error expects a message");
                    {
                        KontFrame frame;
                        frame.kind = KontFrame::Kind::ErrorMessage;
                        frame.expression = list(std::vector<Value>(args.begin() + 1,
                                                                    args.end()));
                        frame.environment = environment;
                        state.frames.push_back(std::move(frame));
                        evaluate(args[0], std::move(environment));
                    }
                    break;
                case CoreForm::ErrorObjectPredicate:
                case CoreForm::ErrorObjectMessage:
                case CoreForm::ErrorObjectIrritants:
                    if (args.size() != 1)
                        throw std::runtime_error("error-object accessor expects one argument");
                    {
                        KontFrame frame;
                        frame.kind = form == CoreForm::ErrorObjectPredicate
                                         ? KontFrame::Kind::ErrorObjectPredicate
                                         : form == CoreForm::ErrorObjectMessage
                                               ? KontFrame::Kind::ErrorObjectMessage
                                               : KontFrame::Kind::ErrorObjectIrritants;
                        state.frames.push_back(std::move(frame));
                        evaluate(args[0], std::move(environment));
                    }
                    break;
                default: {
                    // Generic applications are evaluated left-to-right.  The
                    // operator and each argument are explicit machine states.
                    KontFrame frame;
                    frame.kind = KontFrame::Kind::CallOperator;
                    frame.expression = pair_expression->cdr;
                    frame.environment = environment;
                    state.frames.push_back(std::move(frame));
                    evaluate(pair_expression->car, std::move(environment));
                    break;
                }
                }
                continue;
            }

            if (state.frames.empty())
                return state.values;
            KontFrame frame = std::move(state.frames.back());
            state.frames.pop_back();
            Values produced = std::move(state.values);
            switch (frame.kind) {
            case KontFrame::Kind::Sequence:
                sequence(frame.expression, std::move(frame.environment));
                break;
            case KontFrame::Kind::IfTest: {
                Value test = first_or_unspecified(produced);
                bool truth = !test.is_boolean() || test.as_boolean();
                if (truth) {
                    evaluate(frame.expression, std::move(frame.environment));
                } else if (!frame.auxiliary.is_unspecified()) {
                    evaluate(frame.auxiliary, std::move(frame.environment));
                } else {
                    return_values({Value::unspecified()});
                }
                break;
            }
            case KontFrame::Kind::WhenTest: {
                Value test = first_or_unspecified(produced);
                bool truth = !test.is_boolean() || test.as_boolean();
                bool when = frame.auxiliary.as_boolean();
                if (truth == when)
                    sequence(frame.expression, std::move(frame.environment));
                else
                    return_values({Value::unspecified()});
                break;
            }
            case KontFrame::Kind::DefineValue:
                frame.environment->define(frame.expression,
                                          first_or_unspecified(produced));
                return_values({Value::unspecified()});
                break;
            case KontFrame::Kind::SetValue:
            {
                Value assigned = first_or_unspecified(produced);
                if (frame.expression.is_object() &&
                    frame.expression.as_object()->type() == ObjectType::Pair) {
                    PairObject* target =
                        frame.expression.as_object<PairObject>();
                    CoreForm target_form = core_forms_.lookup(target->car);
                    if (target_form == CoreForm::ModuleRef) {
                        std::vector<Value> reference = proper_list(target->cdr);
                        if (reference.size() != 2)
                            throw std::runtime_error(
                                "module-ref expects module and name");
                        KontFrame module_set;
                        module_set.kind = KontFrame::Kind::SetterTarget;
                        module_set.stage = 2;
                        module_set.values = {assigned};
                        module_set.environment = frame.environment;
                        state.frames.push_back(std::move(module_set));
                        KontFrame target_eval;
                        target_eval.kind = KontFrame::Kind::ModuleReference;
                        target_eval.expression = reference[1];
                        target_eval.environment = frame.environment;
                        target_eval.values = {assigned};
                        state.frames.push_back(std::move(target_eval));
                        evaluate(reference[0], frame.environment);
                        break;
                    }
                    std::vector<Value> target_parts =
                        proper_list(frame.expression);
                    if (target_parts.empty())
                        throw std::runtime_error("set! target is empty");
                    KontFrame setter;
                    setter.kind = KontFrame::Kind::SetterTarget;
                    setter.expression = list(std::vector<Value>(
                        target_parts.begin() + 1, target_parts.end()));
                    setter.environment = frame.environment;
                    setter.values = {assigned};
                    state.frames.push_back(std::move(setter));
                    evaluate(target_parts[0], frame.environment);
                    break;
                }
                try {
                    frame.environment->set(frame.expression, assigned);
                } catch (const UnboundSetError& error) {
                    throw RaisedValue(Value::object(heap_.make<ErrorObject>(
                        error.what(), ValueList{string(error.what())},
                        "unbound-variable")));
                }
                return_values({assigned});
                break;
            }
            case KontFrame::Kind::RaiseValue:
                throw RaisedValue(first_or_unspecified(produced));
            case KontFrame::Kind::RethrowRaised:
                throw RaisedValue(frame.values.front());
            case KontFrame::Kind::RethrowRuntimeError:
                throw std::runtime_error(
                    frame.expression.as_object<StringObject>()->value);
            case KontFrame::Kind::RethrowThrown: {
                ValueList arguments(frame.values.begin(), frame.values.end());
                throw ThrownValue(frame.auxiliary, std::move(arguments));
            }
            case KontFrame::Kind::ErrorMessage: {
                frame.auxiliary = first_or_unspecified(produced);
                if (frame.expression.is_null()) {
                    Value message = frame.auxiliary;
                    std::string text;
                    std::string key;
                    if (message.is_object() &&
                        message.as_object()->type() == ObjectType::String)
                        text = message.as_object<StringObject>()->value;
                    else if (message.is_object() &&
                             message.as_object()->type() == ObjectType::Symbol) {
                        key = message.as_object<SymbolObject>()->name;
                        text = key;
                    } else {
                        throw std::runtime_error(
                            "error message must be a string or symbol");
                    }
                    throw RaisedValue(Value::object(heap_.make<ErrorObject>(
                        text, ValueList{}, key)));
                }
                PairObject* first = frame.expression.as_object<PairObject>();
                frame.kind = KontFrame::Kind::ErrorIrritant;
                frame.expression = first->cdr;
                state.frames.push_back(std::move(frame));
                evaluate(first->car, state.frames.back().environment);
                break;
            }
            case KontFrame::Kind::ErrorIrritant: {
                frame.values.push_back(first_or_unspecified(produced));
                if (!frame.expression.is_null()) {
                    PairObject* next = frame.expression.as_object<PairObject>();
                    frame.expression = next->cdr;
                    state.frames.push_back(std::move(frame));
                    evaluate(next->car, state.frames.back().environment);
                    break;
                }
                Value message = frame.auxiliary;
                std::string text;
                std::string key;
                if (message.is_object() &&
                    message.as_object()->type() == ObjectType::String)
                    text = message.as_object<StringObject>()->value;
                else if (message.is_object() &&
                         message.as_object()->type() == ObjectType::Symbol) {
                    key = message.as_object<SymbolObject>()->name;
                    text = key;
                } else {
                    throw std::runtime_error(
                        "error message must be a string or symbol");
                }
                throw RaisedValue(Value::object(heap_.make<ErrorObject>(
                    text, std::move(frame.values), key)));
            }
            case KontFrame::Kind::ErrorObjectPredicate: {
                Value value = first_or_unspecified(produced);
                return_values({Value::boolean(
                    value.is_object() &&
                    value.as_object()->type() == ObjectType::ErrorObject)});
                break;
            }
            case KontFrame::Kind::ErrorObjectMessage: {
                Value value = first_or_unspecified(produced);
                if (!value.is_object() ||
                    value.as_object()->type() != ObjectType::ErrorObject)
                    throw std::runtime_error("not an error object");
                return_values({string(value.as_object<ErrorObject>()->message)});
                break;
            }
            case KontFrame::Kind::ErrorObjectIrritants: {
                Value value = first_or_unspecified(produced);
                if (!value.is_object() ||
                    value.as_object()->type() != ObjectType::ErrorObject)
                    throw std::runtime_error("not an error object");
                return_values({list_values(
                    value.as_object<ErrorObject>()->irritants)});
                break;
            }
            case KontFrame::Kind::LetBinding:
            case KontFrame::Kind::LetrecBinding: {
                const Value initialized = first_or_unspecified(produced);
                const std::size_t binding_index = frame.index;
                if (frame.kind == KontFrame::Kind::LetBinding) {
                    frame.environment->define(frame.expressions[binding_index],
                                              initialized);
                } else if (frame.stage == 2) {
                    frame.environment->set(frame.expressions[binding_index],
                                           initialized);
                } else {
                    frame.values.push_back(initialized);
                }
                frame.index += 2;
                if (frame.index < frame.expressions.size()) {
                    const Value next_initializer =
                        frame.expressions[frame.index + 1];
                    state.frames.push_back(std::move(frame));
                    const KontFrame& next = state.frames.back();
                    EnvironmentPtr init_environment =
                        next.stage == 0 ? next.secondary_environment
                                        : next.environment;
                    evaluate(next_initializer, std::move(init_environment));
                } else {
                    if (frame.kind == KontFrame::Kind::LetrecBinding &&
                        frame.stage == 1) {
                        for (std::size_t i = 0, j = 0;
                             i < frame.expressions.size(); i += 2, ++j)
                            frame.environment->set(frame.expressions[i],
                                                   frame.values[j]);
                    }
                    sequence(frame.expression, std::move(frame.environment));
                }
                break;
            }
            case KontFrame::Kind::ValuesArgument: {
                frame.values.insert(frame.values.end(), produced.begin(),
                                    produced.end());
                if (frame.expression.is_null()) {
                    return_values(std::move(frame.values));
                } else {
                    PairObject* next = frame.expression.as_object<PairObject>();
                    frame.expression = next->cdr;
                    state.frames.push_back(std::move(frame));
                    evaluate(next->car, state.frames.back().environment);
                }
                break;
            }
            case KontFrame::Kind::CallOperator: {
                frame.auxiliary = first_or_unspecified(produced);
                if (frame.expression.is_null()) {
                    invoke(frame.auxiliary, {});
                } else {
                    KontFrame arguments;
                    arguments.kind = KontFrame::Kind::CallArgument;
                    arguments.expression = frame.expression;
                    arguments.auxiliary = frame.auxiliary;
                    arguments.environment = std::move(frame.environment);
                    state.frames.push_back(std::move(arguments));
                    PairObject* first =
                        state.frames.back().expression.as_object<PairObject>();
                    state.frames.back().expression = first->cdr;
                    evaluate(first->car, state.frames.back().environment);
                }
                break;
            }
            case KontFrame::Kind::CallArgument:
                frame.values.push_back(first_or_unspecified(produced));
                if (frame.expression.is_null()) {
                    invoke(frame.auxiliary, frame.values);
                } else {
                    PairObject* next = frame.expression.as_object<PairObject>();
                    frame.expression = next->cdr;
                    state.frames.push_back(std::move(frame));
                    evaluate(next->car, state.frames.back().environment);
                }
                break;
            case KontFrame::Kind::ApplyProcedure: {
                frame.auxiliary = first_or_unspecified(produced);
                PairObject* first_argument =
                    frame.expression.as_object<PairObject>();
                KontFrame arguments;
                arguments.kind = KontFrame::Kind::ApplyArgument;
                arguments.expression = first_argument->cdr;
                arguments.auxiliary = frame.auxiliary;
                arguments.environment = std::move(frame.environment);
                state.frames.push_back(std::move(arguments));
                evaluate(first_argument->car,
                         state.frames.back().environment);
                break;
            }
            case KontFrame::Kind::ApplyArgument: {
                Value argument = first_or_unspecified(produced);
                if (frame.expression.is_null()) {
                    std::vector<Value> spread = proper_list(argument);
                    frame.values.insert(frame.values.end(), spread.begin(),
                                        spread.end());
                    invoke(frame.auxiliary, frame.values);
                } else {
                    frame.values.push_back(argument);
                    PairObject* next = frame.expression.as_object<PairObject>();
                    frame.expression = next->cdr;
                    state.frames.push_back(std::move(frame));
                    evaluate(next->car, state.frames.back().environment);
                }
                break;
            }
            case KontFrame::Kind::ModuleReference:
                if (frame.stage == 0) {
                    frame.values.push_back(first_or_unspecified(produced));
                    frame.stage = 1;
                    state.frames.push_back(std::move(frame));
                    evaluate(state.frames.back().expression,
                             state.frames.back().environment);
                } else {
                    Value module_set =
                        frame.environment->lookup(symbol("module-set"));
                    invoke(module_set,
                           {frame.values[0], frame.values[1],
                            first_or_unspecified(produced)});
                }
                break;
            case KontFrame::Kind::SetterTarget:
                if (frame.stage == 2) {
                    return_values({frame.values.front()});
                } else if (frame.stage == 0) {
                    frame.auxiliary = first_or_unspecified(produced);
                    if (frame.expression.is_null()) {
                        KontFrame result_frame;
                        result_frame.kind = KontFrame::Kind::SetterTarget;
                        result_frame.stage = 2;
                        result_frame.values = {frame.values.front()};
                        state.frames.push_back(std::move(result_frame));
                        invoke(frame.auxiliary, {frame.values.front()});
                    } else {
                        PairObject* next =
                            frame.expression.as_object<PairObject>();
                        frame.expression = next->cdr;
                        frame.stage = 1;
                        state.frames.push_back(std::move(frame));
                        evaluate(next->car, state.frames.back().environment);
                    }
                } else {
                    frame.values.push_back(first_or_unspecified(produced));
                    if (!frame.expression.is_null()) {
                        PairObject* next =
                            frame.expression.as_object<PairObject>();
                        frame.expression = next->cdr;
                        state.frames.push_back(std::move(frame));
                        evaluate(next->car, state.frames.back().environment);
                    } else {
                        Values setter_arguments(frame.values.begin() + 1,
                                                frame.values.end());
                        setter_arguments.push_back(frame.values.front());
                        KontFrame result_frame;
                        result_frame.kind = KontFrame::Kind::SetterTarget;
                        result_frame.stage = 2;
                        result_frame.values = {frame.values.front()};
                        state.frames.push_back(std::move(result_frame));
                        invoke(frame.auxiliary, setter_arguments);
                    }
                }
                break;
            case KontFrame::Kind::CallWithValuesProducer: {
                if (frame.stage == 1) {
                    invoke(frame.auxiliary, produced);
                    break;
                }
                Value producer = first_or_unspecified(produced);
                KontFrame consumer;
                consumer.kind = KontFrame::Kind::CallWithValuesConsumer;
                consumer.expression = frame.expression;
                consumer.auxiliary = producer;
                consumer.environment = std::move(frame.environment);
                consumer.stage = 1;
                state.frames.push_back(std::move(consumer));
                invoke(producer, {});
                break;
            }
            case KontFrame::Kind::CallWithValuesConsumer:
                if (frame.stage == 1) {
                    frame.values = std::move(produced);
                    frame.stage = 2;
                    state.frames.push_back(std::move(frame));
                    evaluate(state.frames.back().expression,
                             state.frames.back().environment);
                } else {
                    invoke(first_or_unspecified(produced), frame.values);
                }
                break;
            case KontFrame::Kind::ExceptionHandler:
                if (frame.stage == 0) {
                    return_values(std::move(produced));
                } else if (frame.stage == 3) {
                    next_guard_clause(std::move(frame));
                } else if (frame.stage == 1) {
                    bool selected = !first_or_unspecified(produced).is_boolean() ||
                                    first_or_unspecified(produced).as_boolean();
                    if (!selected) {
                        next_guard_clause(std::move(frame));
                    } else if (frame.index != 0) {
                        frame.stage = 2;
                        state.frames.push_back(std::move(frame));
                        evaluate(state.frames.back().expressions.front(),
                                 state.frames.back().secondary_environment);
                    } else {
                        sequence(list(frame.expressions),
                                 std::move(frame.secondary_environment));
                    }
                } else {
                    invoke(first_or_unspecified(produced),
                           {frame.values.front()});
                }
                break;
            case KontFrame::Kind::Catch:
                return_values(std::move(produced));
                break;
            case KontFrame::Kind::PortCallback: {
                if (frame.index != 0) {
                    if (state.winders.empty() ||
                        state.winders.back().bound_port != frame.auxiliary)
                        throw std::runtime_error(
                            "port callback dynamic extent mismatch");
                    DynamicWinder winder = state.winders.back();
                    state.winders.pop_back();
                    leave_winder(winder);
                }
                if (frame.stage != 0) {
                    auto& port = *frame.auxiliary.as_object<OutputPortObject>();
                    auto stream =
                        std::dynamic_pointer_cast<std::ostringstream>(
                            port.stream);
                    if (!stream)
                        throw std::runtime_error(
                            "output string callback has no string buffer");
                    return_values({string(stream->str())});
                } else {
                    return_values(std::move(produced));
                }
                break;
            }
            case KontFrame::Kind::DynamicWind:
                if (frame.stage == 0) {
                    DynamicWinder winder;
                    winder.before = frame.values[0];
                    winder.after = frame.values[1];
                    winder.environment = frame.environment;
                    winder.identity = next_winder_identity_++;
                    state.winders.push_back(winder);
                    frame.stage = 1;
                    state.frames.push_back(std::move(frame));
                    invoke(state.frames.back().expression, {});
                } else if (frame.stage == 1) {
                    if (state.winders.empty())
                        throw std::runtime_error("dynamic-wind stack mismatch");
                    state.winders.pop_back();
                    frame.auxiliary = frame.values[1];
                    frame.values = std::move(produced);
                    frame.stage = 2;
                    state.frames.push_back(std::move(frame));
                    invoke(state.frames.back().auxiliary, {});
                } else {
                    return_values(std::move(frame.values));
                }
                break;
            case KontFrame::Kind::MapLoop:
                if (produced.size() != 1)
                    throw std::runtime_error(
                        "map procedure returned multiple values");
                frame.values.push_back(produced.front());
                for (Value& iterator : frame.expressions)
                    iterator = iterator.as_object<PairObject>()->cdr;
                advance_hof(std::move(frame));
                break;
            case KontFrame::Kind::ForEachLoop:
                for (Value& iterator : frame.expressions)
                    iterator = iterator.as_object<PairObject>()->cdr;
                advance_hof(std::move(frame));
                break;
            case KontFrame::Kind::FoldLoop:
                if (produced.size() != 1)
                    throw std::runtime_error(
                        "fold procedure returned multiple values");
                frame.values = {produced.front()};
                ++frame.index;
                advance_hof(std::move(frame));
                break;
            case KontFrame::Kind::FilterLoop:
                if (produced.size() != 1)
                    throw std::runtime_error(
                        "filter predicate returned multiple values");
                if (!produced.front().is_boolean() ||
                    produced.front().as_boolean())
                    frame.values.push_back(
                        frame.expressions[frame.index]);
                ++frame.index;
                advance_hof(std::move(frame));
                break;
            case KontFrame::Kind::AnyLoop:
                if (produced.size() != 1)
                    throw std::runtime_error(
                        "any predicate returned multiple values");
                if (!produced.front().is_boolean() ||
                    produced.front().as_boolean())
                    return_values({produced.front()});
                else {
                    ++frame.index;
                    advance_hof(std::move(frame));
                }
                break;
            case KontFrame::Kind::EveryLoop:
                if (produced.size() != 1)
                    throw std::runtime_error(
                        "every predicate returned multiple values");
                if (produced.front().is_boolean() &&
                    !produced.front().as_boolean())
                    return_values({Value::boolean(false)});
                else {
                    frame.values = {produced.front()};
                    ++frame.index;
                    advance_hof(std::move(frame));
                }
                break;
            case KontFrame::Kind::MemberLoop:
                if (produced.size() != 1)
                    throw std::runtime_error(
                        "member comparator returned multiple values");
                if (!produced.front().is_boolean() ||
                    produced.front().as_boolean())
                    return_values({frame.expressions.front()});
                else {
                    frame.expressions.front() =
                        frame.expressions.front()
                            .as_object<PairObject>()->cdr;
                    frame.values.clear();
                    advance_hof(std::move(frame));
                }
                break;
            case KontFrame::Kind::AssocLoop:
                if (produced.size() != 1)
                    throw std::runtime_error(
                        "assoc comparator returned multiple values");
                if (!produced.front().is_boolean() ||
                    produced.front().as_boolean())
                    return_values(frame.values);
                else {
                    frame.expressions.front() =
                        frame.expressions.front()
                            .as_object<PairObject>()->cdr;
                    frame.values.clear();
                    advance_hof(std::move(frame));
                }
                break;
            case KontFrame::Kind::VectorFilterLoop:
                if (produced.empty())
                    throw std::runtime_error(
                        "g_vector_filter predicate returned no value");
                if (!produced.front().is_boolean() ||
                    produced.front().as_boolean())
                    frame.values.push_back(
                        frame.expressions[frame.index]);
                ++frame.index;
                advance_hof(std::move(frame));
                break;
            case KontFrame::Kind::ContinuationTransfer:
                continue_transfer(std::move(frame));
                break;
            default:
                throw std::runtime_error("unimplemented evaluator continuation frame");
            }
        }
    } catch (const ContinuationJump& jump) {
        if (!jump.continuation.is_object() ||
            jump.continuation.as_object()->type() != ObjectType::Continuation)
            throw std::runtime_error("attempt to apply non-continuation");
        const EvalSnapshot& target =
            jump.continuation.as_object<ContinuationObject>()->snapshot;
        if (target.machine_id != state.machine_id) {
            for (const EvalSnapshot* active : active_evaluations_) {
                if (active && active != &state &&
                    active->machine_id == target.machine_id)
                    throw;
            }
        }
        std::size_t common = 0;
        while (common < state.winders.size() && common < target.winders.size() &&
               state.winders[common].identity == target.winders[common].identity)
            ++common;
        KontFrame transfer;
        transfer.kind = KontFrame::Kind::ContinuationTransfer;
        transfer.auxiliary = jump.continuation;
        transfer.values = jump.values;
        for (std::size_t i = state.winders.size(); i > common; --i)
            transfer.exiting_winders.push_back(state.winders[i - 1]);
        for (std::size_t i = common; i < target.winders.size(); ++i)
            transfer.entering_winders.push_back(target.winders[i]);
        state.frames.clear();
        continue_transfer(std::move(transfer));
        goto restart_machine;
    } catch (const ThrownValue& thrown) {
        Value info = Value::null();
        for (auto it = thrown.arguments().rbegin();
             it != thrown.arguments().rend(); ++it)
            info = pair(*it, info);
        if (route_to_catch(thrown.tag(), info)) goto restart_machine;
        if (state.winders.empty()) throw;
        EvalSnapshot target;
        target.machine_id = state.machine_id;
        KontFrame rethrow;
        rethrow.kind = KontFrame::Kind::RethrowThrown;
        rethrow.auxiliary = thrown.tag();
        rethrow.values.assign(thrown.arguments().begin(),
                              thrown.arguments().end());
        target.frames.push_back(std::move(rethrow));
        target.values = {Value::unspecified()};
        target.returning = true;
        Value continuation = Value::object(
            heap_.make<ContinuationObject>(std::move(target)));
        KontFrame transfer;
        transfer.kind = KontFrame::Kind::ContinuationTransfer;
        transfer.auxiliary = continuation;
        transfer.values = {Value::unspecified()};
        for (auto it = state.winders.rbegin(); it != state.winders.rend(); ++it)
            transfer.exiting_winders.push_back(*it);
        state.frames.clear();
        continue_transfer(std::move(transfer));
        goto restart_machine;
    } catch (const RaisedValue& raised) {
        Value tag = Value::boolean(true);
        Value info = pair(raised.value(), Value::null());
        if (raised.value().is_object() &&
            raised.value().as_object()->type() == ObjectType::ErrorObject) {
            const auto* error = raised.value().as_object<ErrorObject>();
            if (!error->key.empty()) {
                tag = symbol(error->key);
                info = Value::null();
                for (auto it = error->irritants.rbegin();
                     it != error->irritants.rend(); ++it)
                    info = pair(*it, info);
            }
        }
        if (route_to_catch(tag, info, true)) goto restart_machine;
        std::size_t handler_index = state.frames.size();
        while (handler_index > 0) {
            const KontFrame& candidate = state.frames[handler_index - 1];
            if (candidate.kind == KontFrame::Kind::ExceptionHandler &&
                candidate.stage == 0)
                break;
            --handler_index;
        }
        if (handler_index == 0) {
            if (state.winders.empty()) throw;
            unwind_before_rethrow(true, raised.value(), {});
            goto restart_machine;
        }
        route_to_guard(handler_index, raised.value());
        goto restart_machine;
    } catch (const std::runtime_error& error) {
        Value tag = runtime_error_tag(error.what());
        Value info = pair(string(error.what()), Value::null());
        if (route_to_catch(tag, info, true)) goto restart_machine;
        std::size_t handler_index = state.frames.size();
        while (handler_index > 0) {
            const KontFrame& candidate = state.frames[handler_index - 1];
            if (candidate.kind == KontFrame::Kind::ExceptionHandler &&
                candidate.stage == 0)
                break;
            --handler_index;
        }
        if (handler_index == 0) {
            if (state.winders.empty()) throw;
            unwind_before_rethrow(false, Value::unspecified(), error.what());
            goto restart_machine;
        }
        Value caught = Value::object(
            heap_.make<ErrorObject>(error.what(), ValueList{}));
        route_to_guard(handler_index, caught);
        goto restart_machine;
    } catch (const std::logic_error& error) {
        Value tag = runtime_error_tag(error.what());
        Value info = pair(string(error.what()), Value::null());
        if (route_to_catch(tag, info, true)) goto restart_machine;
        std::size_t handler_index = state.frames.size();
        while (handler_index > 0) {
            const KontFrame& candidate = state.frames[handler_index - 1];
            if (candidate.kind == KontFrame::Kind::ExceptionHandler &&
                candidate.stage == 0)
                break;
            --handler_index;
        }
        if (handler_index == 0) {
            if (state.winders.empty()) throw;
            unwind_before_rethrow(false, Value::unspecified(), error.what());
            goto restart_machine;
        }
        Value caught = Value::object(
            heap_.make<ErrorObject>(error.what(), ValueList{}));
        route_to_guard(handler_index, caught);
        goto restart_machine;
    }
}

Values Evaluator::apply(Value procedure, const Values& arguments) {
    if (!procedure.is_object()) {
        trace_throw("apply-non-procedure");
        throw std::runtime_error("attempt to apply non-procedure");
    }

    Object* object = procedure.as_object();
    if (object->type() == ObjectType::Primitive)
        return procedure.as_object<PrimitiveObject>()->function(arguments);

    if (object->type() != ObjectType::Closure) {
        trace_throw("apply-non-procedure");
        throw std::runtime_error("attempt to apply non-procedure");
    }

    Value next_procedure = procedure;
    Values next_arguments = arguments;
    for (;;) {
        if (!next_procedure.is_object()) {
            trace_throw("apply-non-procedure");
            throw std::runtime_error("attempt to apply non-procedure");
        }
        Object* next_object = next_procedure.as_object();
        if (next_object->type() == ObjectType::Primitive)
            return next_procedure.as_object<PrimitiveObject>()->function(
                next_arguments);
        if (next_object->type() != ObjectType::Closure) {
            trace_throw("apply-non-procedure");
            throw std::runtime_error("attempt to apply non-procedure");
        }

        ClosureObject* closure = next_procedure.as_object<ClosureObject>();
        EnvironmentPtr call_environment =
            std::make_shared<Environment>(closure->environment);
        std::vector<Value> required;
        Value formals = closure->formals;
        Value rest = Value::null();
        while (!formals.is_null()) {
            if (!formals.is_object() ||
                formals.as_object()->type() != ObjectType::Pair) {
                rest = formals;
                break;
            }
            PairObject* formal_pair = formals.as_object<PairObject>();
            required.push_back(formal_pair->car);
            formals = formal_pair->cdr;
        }
        if (next_arguments.size() < required.size() ||
            (rest.is_null() && next_arguments.size() != required.size())) {
            std::string message =
                "wrong number of arguments: expected " +
                std::to_string(required.size()) +
                (rest.is_null() ? "" : " or more") + ", got " +
                std::to_string(next_arguments.size());
            if (!required.empty() && required[0].is_object() &&
                required[0].as_object()->type() == ObjectType::Symbol)
                message += "; first formal: " +
                           required[0].as_object<SymbolObject>()->name;
            throw std::runtime_error(message);
        }
        for (std::size_t i = 0; i < required.size(); ++i)
            call_environment->define(required[i], next_arguments[i]);
        if (!rest.is_null())
            call_environment->define(
                rest, list_values(Values(next_arguments.begin() + required.size(),
                                         next_arguments.end())));
        Values body_result = eval_tail_sequence(closure->body, call_environment);
        if (has_pending_call_) {
            // The body ended in another closure call: keep iterating instead
            // of recursing (or unwinding) per call.
            has_pending_call_ = false;
            next_procedure = pending_procedure_;
            next_arguments = std::move(pending_arguments_);
            continue;
        }
        return body_result;
    }
}

Values Evaluator::apply_values(Value procedure, const Values& arguments) {
    EvalSnapshot state;
    state.machine_id = next_machine_id_++;
    state.applying = true;
    state.initial_procedure = procedure;
    state.initial_arguments = arguments;
    active_evaluations_.push_back(&state);
    struct PopActive final {
        std::vector<EvalSnapshot*>& active;
        ~PopActive() { active.pop_back(); }
    } pop{active_evaluations_};
    return run_machine(state);
}

} // namespace goldfish::runtime
