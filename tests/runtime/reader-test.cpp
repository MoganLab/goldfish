#include "runtime/artifact.hpp"
#include "runtime/reader.hpp"
#include "runtime/runtime.hpp"
#include "runtime/standard_primitives.hpp"

#include <cassert>
#include <fstream>
#include <iterator>
#include <stdexcept>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    Evaluator& evaluator = runtime.evaluator();
    install_standard_primitives(evaluator);

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
