#include "runtime/bootstrap.hpp"

#include <cassert>

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
}
