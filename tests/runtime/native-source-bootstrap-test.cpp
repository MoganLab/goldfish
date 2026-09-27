#include "runtime/bootstrap.hpp"

#include <cassert>
#include <iostream>
#include <cstdlib>
#include <stdexcept>
#include <string>

using namespace goldfish::runtime;

int main(int argc, char** argv) {
  Runtime runtime;
  NativeBootstrap bootstrap(runtime);
  try {
    setenv("GOLDFISH_NATIVE_ARTIFACTS", "1", 1);
    bootstrap.install_primitives();
    // The native bootstrap keeps only artifact compatibility aliases.  The
    // s7 inlet/let object family belongs to the explicit migration bridge.
    bool legacy_surface_absent = false;
    try {
      (void)runtime.evaluator().eval(runtime.evaluator().symbol("inlet"));
    } catch (const std::runtime_error&) {
      legacy_surface_absent = true;
    }
    assert(legacy_surface_absent);
    bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
    if (argc == 1 || (argc == 2 && std::string(argv[1]) == "--rebuild"))
      bootstrap.load_cached_runtime();
    else
      for (int i = 1; i < argc; ++i)
        bootstrap.load_artifact(argv[i]);

    Evaluator& evaluator = runtime.evaluator();
    bootstrap.install_source_expander();
    Value load_source = evaluator.eval(evaluator.symbol("load-source-file"));
    evaluator.apply_values(load_source,
                           {evaluator.string("expander/lib/install.scm")});
    bootstrap.install_expansion_helpers();
    if (argc == 2 && std::string(argv[1]) == "--rebuild") {
      evaluator.apply_values(
          load_source,
          {evaluator.string("goldfish/expander/build-combined.scm")});
      return 0;
    }
    if (argc != 1)
      throw std::runtime_error("usage: native-source-bootstrap-test [--rebuild]");
    Value read_forms = evaluator.eval(evaluator.symbol("read-forms"));
    Value input = evaluator.apply_values(
        evaluator.eval(evaluator.symbol("open-input-file")),
        {evaluator.string("tests/runtime/fixtures/native-source-bootstrap.scm")})[0];
    Value forms = evaluator.apply_values(read_forms, {input})[0];
    assert(forms.is_object());

    Value compile_file = evaluator.eval(evaluator.symbol("compile-file"));
    Value lowered = evaluator.apply_values(
        compile_file,
        {evaluator.string("tests/runtime/fixtures/native-source-bootstrap.scm")})[0];
    assert(evaluator.eval(lowered).as_integer() == 42);


  } catch (const RaisedValue& raised) {
    if (raised.value().is_object() &&
        raised.value().as_object()->type() == ObjectType::ErrorObject)
      {
        const auto* error = raised.value().as_object<ErrorObject>();
        std::cerr << "scheme error: " << error->message;
        for (Value irritant : error->irritants) {
          std::cerr << " ";
          if (irritant.is_object() &&
              irritant.as_object()->type() == ObjectType::String)
            std::cerr << runtime.evaluator().string_value(irritant);
          else if (irritant.is_object() &&
                   irritant.as_object()->type() == ObjectType::Symbol)
            std::cerr << irritant.as_object<SymbolObject>()->name;
          else if (irritant.is_integer())
            std::cerr << irritant.as_integer();
          else
            std::cerr << "<value>";
        }
        std::cerr << "\n";
        return 1;
      }
    throw;
  } catch (const std::exception& error) {
    std::cerr << "error: " << error.what() << "\n";
    return 1;
  }
}
