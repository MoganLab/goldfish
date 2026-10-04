#include "runtime/bootstrap.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <unistd.h>

using namespace goldfish::runtime;
namespace fs = std::filesystem;

int main() {
    fs::path root = fs::temp_directory_path() /
        ("goldfish-cache-test-" + std::to_string(getpid()));
    try {
        fs::create_directories(root / "sources/expander/lib");
        fs::create_directories(root / "sources/compiler");
        fs::create_directories(root / "sources/scheme");
        auto put = [](const fs::path& path, const std::string& text) {
            fs::create_directories(path.parent_path());
            std::ofstream out(path); out << text;
            if (!out) throw std::runtime_error("cannot write fixture");
        };
        put(root / "engine", "runtime-v1");
        put(root / "sources/expander/kernel-combined.scm", "kernel-v1");
        put(root / "sources/dep.scm", "dep-v1");
        const char* sources[] = {
            "expander/lib/syntax-runtime.scm", "expander/lib/syntax-case.scm",
            "expander/lib/define-record-type.scm", "expander/lib/core-macros.scm",
            "expander/lib/cond-expand.scm", "expander/lib/defmacro.scm",
            "expander/lib/define-star.scm", "expander/lib/module-registry.scm",
            "expander/lib/module.scm", "expander/lib/standard.scm",
            "scheme/case-lambda.scm", "scheme/base.scm"
        };
        for (const char* source : sources) put(root / "sources" / source, "source-v1");
        Runtime runtime;
        NativeBootstrap bootstrap(runtime);
        bootstrap.install_primitives();
        auto& evaluator = runtime.evaluator();
        evaluator.global_environment()->define(evaluator.symbol("*load-path*"),
            evaluator.list({evaluator.string((root / "sources").string())}));
        evaluator.define_primitive("g_executable", [&](const Values&) {
            return Values{evaluator.string((root / "engine").string())};
        });
        auto call = [&](const char* name, const Values& args) {
            return evaluator.apply_values(evaluator.eval(evaluator.symbol(name)), args)[0];
        };
        auto stamp = [&](const fs::path& path) {
            Value p = evaluator.string(path.string());
            return Values{call("g_path-getmtime", {p}), call("g_path-getsize", {p}),
                          call("g_md5-by-file", {p})};
        };
        auto render = [&](Value value) {
            Value port = call("open-output-string", {});
            call("write", {value, port});
            return evaluator.string_value(call("get-output-string", {port}));
        };
        const fs::path cache = root / "cache";
        const fs::path version = bootstrap.cache_directory(cache.string());
        auto write_artifact = [&](const char* source) {
            auto values = stamp(root / "sources" / source);
            auto kernel = stamp(root / "sources/expander/kernel-combined.scm");
            values.insert(values.end(), kernel.begin(), kernel.end());
            values.push_back(evaluator.symbol("engine-abi"));
            values.push_back(Value::integer(1));
            auto dep = stamp(root / "sources/dep.scm");
            dep.insert(dep.begin(), evaluator.list({evaluator.symbol("dep")}));
            Value bundle = evaluator.list({evaluator.symbol("bundle"), Value::integer(1),
                evaluator.symbol("module"), evaluator.list({evaluator.symbol("defs")}),
                evaluator.list({evaluator.symbol("macros")}),
                evaluator.list({evaluator.symbol("bindings")})});
            Value record = evaluator.list({evaluator.symbol("gfo"), Value::integer(0),
                evaluator.list(values), bundle, evaluator.list({evaluator.list(dep)})});
            put(version / (std::string(source) + "-o2.gfo"), render(record));
        };
        for (const char* source : sources) write_artifact(source);
        auto require_valid = [&] {
            if (bootstrap.validate_cached_runtime(cache.string()) != version.string())
                throw std::runtime_error("selected a different cache version");
        };
        auto require_invalid = [&] {
            bool rejected = false;
            try { bootstrap.validate_cached_runtime(cache.string()); }
            catch (const std::runtime_error&) { rejected = true; }
            if (!rejected) throw std::runtime_error("accepted an invalid cache");
        };
        require_valid();
        fs::copy(version, cache / "vnewer-but-incompatible", fs::copy_options::recursive);
        require_valid();
        if (bootstrap.validate_cached_runtime(version.string()) != version.string())
            throw std::runtime_error("explicit current version rejected");
        auto artifact = version / "scheme/base.scm-o2.gfo";
        fs::remove(artifact); require_invalid(); write_artifact("scheme/base.scm");
        put(artifact, "(gfo 0 ("); require_invalid(); write_artifact("scheme/base.scm");
        { std::ofstream out(artifact, std::ios::app); out << " #f"; }
        require_invalid(); write_artifact("scheme/base.scm");
        put(artifact, "(gfo 99 () (bundle 1 module (defs)))");
        require_invalid(); write_artifact("scheme/base.scm");
        auto dep = root / "sources/dep.scm";
        auto time = fs::last_write_time(dep);
        put(dep, "dep-v2"); fs::last_write_time(dep, time); require_invalid();
        put(dep, "dep-v1"); fs::last_write_time(dep, time); require_valid();
        fs::rename(dep, root / "dep.saved"); require_invalid();
        fs::rename(root / "dep.saved", dep); require_valid();
        auto source = root / "sources/expander/lib/standard.scm";
        time = fs::last_write_time(source);
        put(source, "source-v2"); fs::last_write_time(source, time); require_invalid();
        put(source, "source-v1"); fs::last_write_time(source, time); require_valid();
        put(root / "sources/compiler/new.scm", "new input"); require_invalid();
        fs::remove(root / "sources/compiler/new.scm"); require_valid();
        put(root / "engine", "runtime-v2"); require_invalid();
        put(root / "engine", "runtime-v1"); require_valid();
        put(root / "sources/expander/kernel-combined.scm", "kernel-v2"); require_invalid();
        fs::remove_all(root);
        std::cout << "native cache freshness checks passed\n";
    } catch (const std::exception& error) {
        fs::remove_all(root);
        std::cerr << error.what() << '\n';
        return 1;
    }
}
