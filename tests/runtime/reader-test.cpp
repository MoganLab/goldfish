#include "runtime/artifact.hpp"
#include "runtime/reader.hpp"
#include "runtime/runtime.hpp"
#include "runtime/standard_primitives.hpp"

#include <cassert>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <utility>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    Evaluator& evaluator = runtime.evaluator();
    install_runtime_primitives(evaluator);

    TinyReader reader(
        evaluator,
        "; comment\n(begin (define x 40) (+ x 2)) '(a . b) \"line\\ntext\" #(1 #\\a)");
    Value program = *reader.read();
    assert(evaluator.eval(program).as_integer() == 42);

    Value dotted = *reader.read();
    assert(dotted.as_object<PairObject>()->car == evaluator.symbol("quote"));
    Value quoted = evaluator.eval(dotted);
    assert(quoted.as_object<PairObject>()->cdr == evaluator.symbol("b"));

    Value string = *reader.read();
    assert(evaluator.string_value(string) == "line\ntext");
    Value vector = *reader.read();
    assert(evaluator.vector_values(vector).size() == 2);
    assert(evaluator.character_value(evaluator.vector_values(vector)[1]) == U'a');
    assert(!reader.read());

    for (const auto& shorthand : {std::pair<const char*, const char*>("#'x", "syntax"),
                                  {"#`x", "quasisyntax"},
                                  {"#,x", "unsyntax"},
                                  {"#,@x", "unsyntax-splicing"}}) {
        TinyReader syntax_reader(evaluator, shorthand.first);
        Value form = *syntax_reader.read();
        assert(form.as_object<PairObject>()->car ==
               evaluator.symbol(shorthand.second));
    }

    TinyReader unicode_character_reader(evaluator, "#\\x3007");
    assert(evaluator.character_value(*unicode_character_reader.read()) ==
           static_cast<char32_t>(0x3007));
    TinyReader utf8_character_reader(evaluator, "#\\µ");
    assert(evaluator.character_value(*utf8_character_reader.read()) ==
           static_cast<char32_t>(0x00b5));

    Value read = evaluator.eval(evaluator.symbol("read"));
    Value port = evaluator.apply_values(
        evaluator.eval(evaluator.symbol("open-input-string")),
        {evaluator.string(" ; port comment\n(1 . 2) 7")})[0];
    Value first = evaluator.apply_values(read, {port})[0];
    assert(first.as_object<PairObject>()->car.as_integer() == 1);
    assert(first.as_object<PairObject>()->cdr.as_integer() == 2);
    assert(evaluator.apply_values(read, {port})[0].as_integer() == 7);
    assert(evaluator.apply_values(read, {port})[0].is_object());
    assert(evaluator.apply_values(
               evaluator.eval(evaluator.symbol("eof-object?")),
               {evaluator.apply_values(read, {port})[0]})[0].as_boolean());

    Value file_port = evaluator.apply_values(
        evaluator.eval(evaluator.symbol("open-input-file")),
        {evaluator.string("tests/runtime/fixtures/lowered-artifact.scm")})[0];
    assert(evaluator.apply_values(read, {file_port})[0].is_object());
    evaluator.apply_values(evaluator.eval(evaluator.symbol("close-input-port")),
                           {file_port});
    try {
        (void)evaluator.apply_values(read, {file_port});
        assert(false);
    } catch (const std::runtime_error&) {
    }

    ArtifactLoader loader(evaluator);
    assert(loader.load_file("tests/runtime/fixtures/lowered-artifact.scm")
               .as_integer() == 42);
    assert(loader.load_gfo_file("tests/runtime/fixtures/lowered-artifact.gfo")
               .as_integer() == 42);

    std::ifstream kernel_file("goldfish/expander/kernel-combined.scm");
    assert(kernel_file);
    std::string kernel_source((std::istreambuf_iterator<char>(kernel_file)),
                              std::istreambuf_iterator<char>());
    TinyReader kernel_reader(evaluator, std::move(kernel_source));
    Value kernel = *kernel_reader.read();
    assert(kernel.as_object<PairObject>()->car == evaluator.symbol("begin"));
    assert(!kernel_reader.read());
}
