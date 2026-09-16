#include "runtime/bootstrap.hpp"

#include <cassert>
#include <iostream>

int main(int argc, char** argv) {
    assert(argc >= 2);
    goldfish::runtime::Runtime runtime;
    goldfish::runtime::NativeBootstrap bootstrap(runtime);
    bootstrap.install_primitives();
    bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
    try {
        for (int i = 1; i < argc; ++i)
            bootstrap.load_artifact(argv[i]);
    } catch (const goldfish::runtime::RaisedValue& raised) {
        auto value = raised.value();
        if (value.is_object() && value.as_object()->type() ==
                                    goldfish::runtime::ObjectType::ErrorObject)
            std::cerr << value.as_object<goldfish::runtime::ErrorObject>()->message
                      << "\n";
        throw;
    }
}
