#include "runtime/bootstrap.hpp"

#include <cassert>
#include <string>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    NativeBootstrap bootstrap(runtime);
    bootstrap.install_primitives();
    bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
    bootstrap.register_library(
        "(dep)", "tests/runtime/fixtures/native-dependency.gfo");
    bootstrap.register_library(
        "(dependent)", "tests/runtime/fixtures/native-dependent.gfo");
    bootstrap.load_library("(dependent)");
    assert(runtime.evaluator().eval(runtime.evaluator().symbol("result"))
               .as_integer() == 7);

    bootstrap.register_library(
        "(missing)", "tests/runtime/fixtures/no-such.gfo");
    for (int attempt = 0; attempt != 2; ++attempt) {
        try {
            bootstrap.load_library("(missing)");
            assert(false);
        } catch (const std::runtime_error& error) {
            assert(std::string(error.what()).find("cycle") ==
                   std::string::npos);
        }
    }
    try {
        bootstrap.register_library("(missing)", "other.gfo");
        assert(false);
    } catch (const std::runtime_error&) {
    }
}
