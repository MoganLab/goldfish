#include "runtime/platform_primitives.hpp"

#include <array>
#include <chrono>
#include <cstdlib>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>

#include <tbox/hash/md5.h>
#include <tbox/hash/sha.h>

#if !defined(_WIN32)
extern char** environ;
#endif

namespace goldfish::runtime {

namespace {

namespace fs = std::filesystem;

void require_arity(const Values& args, std::size_t count, const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

void install(Evaluator& evaluator, const char* name,
             PrimitiveObject::Function function) {
    evaluator.define_primitive(name, std::move(function));
}

std::string digest_hex(const unsigned char* bytes, std::size_t size) {
    static constexpr char digits[] = "0123456789abcdef";
    std::string result;
    result.reserve(size * 2);
    for (std::size_t i = 0; i < size; ++i) {
        result.push_back(digits[bytes[i] >> 4]);
        result.push_back(digits[bytes[i] & 0x0f]);
    }
    return result;
}

template <typename Update, typename Finish>
std::optional<std::string> digest_file(const std::string& path,
                                       Update&& update,
                                       Finish&& finish,
                                       std::size_t digest_size) {
    std::ifstream input(path, std::ios::binary);
    if (!input) return std::nullopt;
    std::array<unsigned char, 4096> buffer{};
    while (input) {
        input.read(reinterpret_cast<char*>(buffer.data()), buffer.size());
        std::streamsize count = input.gcount();
        if (count > 0) update(buffer.data(), static_cast<std::size_t>(count));
    }
    if (!input.eof()) return std::nullopt;
    std::array<unsigned char, 32> digest{};
    finish(digest.data(), digest_size);
    return digest_hex(digest.data(), digest_size);
}

std::optional<std::string> md5_file(const std::string& path) {
    tb_md5_t state;
    tb_md5_init(&state, 0);
    return digest_file(
        path,
        [&state](const unsigned char* data, std::size_t size) {
            tb_md5_spak(&state, data, static_cast<tb_size_t>(size));
        },
        [&state](unsigned char* digest, std::size_t size) {
            tb_md5_exit(&state, digest, static_cast<tb_size_t>(size));
        },
        16);
}

std::optional<std::string> sha256_file(const std::string& path) {
    tb_sha_t state;
    tb_sha_init(&state, 256);
    return digest_file(
        path,
        [&state](const unsigned char* data, std::size_t size) {
            tb_sha_spak(&state, data, static_cast<tb_size_t>(size));
        },
        [&state](unsigned char* digest, std::size_t size) {
            tb_sha_exit(&state, digest, static_cast<tb_size_t>(size));
        },
        32);
}

} // namespace

void install_platform_primitives(Evaluator& evaluator) {
    install(evaluator, "getenv", [&evaluator](const Values& args) {
        require_arity(args, 1, "getenv");
        const char* value = std::getenv(evaluator.string_value(args[0]).c_str());
        return Values{value ? evaluator.string(value) : Value::boolean(false)};
    });
    install(evaluator, "g_path-getsize", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_path-getsize");
        std::error_code error;
        auto size = fs::file_size(evaluator.string_value(args[0]), error);
        return Values{error ? Value::integer(0)
                            : Value::integer(static_cast<std::int64_t>(size))};
    });
    install(evaluator, "g_path-getmtime", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_path-getmtime");
        std::error_code error;
        auto time = fs::last_write_time(evaluator.string_value(args[0]), error);
        if (error) return Values{Value::integer(0)};
        return Values{Value::integer(static_cast<std::int64_t>(
            time.time_since_epoch().count()))};
    });
    install(evaluator, "g_md5-by-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_md5-by-file");
        // The cache stamp uses the same MD5 spelling as the legacy host.
        // Returning #f here turns every native start into a cold expansion.
        auto digest = md5_file(args[0].as_object<StringObject>()->value);
        return Values{digest ? Value::object(
                                   evaluator.heap().make<StringObject>(*digest))
                               : Value::boolean(false)};
    });
    // Small filesystem substrate used while the Scheme cache layer is
    // bootstrapping.  Hashing is deliberately left to the later cache
    // backend; a stable textual fallback keeps the cold bootstrap usable.
    install(evaluator, "g_load-path", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_load-path");
        return Values{evaluator.list({evaluator.string("."),
                                      evaluator.string("goldfish")})};
    });
    install(evaluator, "g_executable", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_executable");
        return Values{evaluator.string("bin/gf")};
    });
    install(evaluator, "g_getpid", [](const Values& args) {
        require_arity(args, 0, "g_getpid");
        return Values{Value::integer(0)};
    });
    install(evaluator, "g_get-environment-variable", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_get-environment-variable");
        const char* value = std::getenv(evaluator.string_value(args[0]).c_str());
        return Values{value ? evaluator.string(value) : Value::boolean(false)};
    });
    install(evaluator, "g_getenvs", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_getenvs");
        std::vector<Value> entries;
#if !defined(_WIN32)
        for (char** item = environ; item && *item; ++item) {
            const std::string entry(*item);
            const std::size_t split = entry.find('=');
            if (split != std::string::npos)
                entries.push_back(evaluator.pair(
                    evaluator.string(entry.substr(0, split)),
                    evaluator.string(entry.substr(split + 1))));
        }
#endif
        return Values{evaluator.list(entries)};
    });
    install(evaluator, "g_command-line", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_command-line");
        return Values{evaluator.list({evaluator.string("goldfish")})};
    });
    for (const char* name : {"exit", "emergency-exit"}) {
        install(evaluator, name, [name](const Values& args) {
            if (args.size() > 1)
                throw std::runtime_error(std::string(name) +
                                         " expects zero or one arguments");
            std::exit(args.empty() || !args[0].is_integer()
                          ? 0 : static_cast<int>(args[0].as_integer()));
            return Values{Value::unspecified()};
        });
    }
    install(evaluator, "g_get-time-of-day", [](const Values& args) {
        require_arity(args, 0, "g_get-time-of-day");
        using namespace std::chrono;
        const auto now = system_clock::now().time_since_epoch();
        const auto micros = duration_cast<microseconds>(now).count();
        return Values{Value::integer(micros / 1000000),
                      Value::integer(micros % 1000000)};
    });
    install(evaluator, "g_datetime-now", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_datetime-now");
        const std::time_t now = std::time(nullptr);
        std::tm local{};
#if defined(_WIN32)
        localtime_s(&local, &now);
#else
        localtime_r(&now, &local);
#endif
        return Values{evaluator.vector({
            Value::integer(local.tm_year + 1900),
            Value::integer(local.tm_mon + 1),
            Value::integer(local.tm_mday),
            Value::integer(local.tm_hour),
            Value::integer(local.tm_min),
            Value::integer(local.tm_sec),
            Value::integer(local.tm_isdst)})};
    });
    install(evaluator, "g_monotonic-nanosecond", [](const Values& args) {
        require_arity(args, 0, "g_monotonic-nanosecond");
        using namespace std::chrono;
        return Values{Value::integer(duration_cast<nanoseconds>(
            steady_clock::now().time_since_epoch()).count())};
    });
    install(evaluator, "g_process-cpu-nanosecond", [](const Values& args) {
        require_arity(args, 0, "g_process-cpu-nanosecond");
        return Values{Value::integer(0)};
    });
    install(evaluator, "g_thread-cpu-nanosecond", [](const Values& args) {
        require_arity(args, 0, "g_thread-cpu-nanosecond");
        return Values{Value::integer(0)};
    });
    install(evaluator, "g_system-clock-resolution", [](const Values& args) {
        require_arity(args, 0, "g_system-clock-resolution");
        return Values{Value::integer(1)};
    });
    install(evaluator, "g_steady-clock-resolution", [](const Values& args) {
        require_arity(args, 0, "g_steady-clock-resolution");
        return Values{Value::integer(1)};
    });
    install(evaluator, "g_process-clock-resolution", [](const Values& args) {
        require_arity(args, 0, "g_process-clock-resolution");
        return Values{Value::integer(1)};
    });
    install(evaluator, "g_thread-clock-resolution", [](const Values& args) {
        require_arity(args, 0, "g_thread-clock-resolution");
        return Values{Value::integer(1)};
    });
    install(evaluator, "g_listdir", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_listdir");
        std::error_code error;
        std::vector<Value> entries;
        for (const auto& entry : fs::directory_iterator(
                 evaluator.string_value(args[0]), error))
            entries.push_back(evaluator.string(entry.path().filename().string()));
        if (error) return Values{Value::boolean(false)};
        return Values{evaluator.vector(entries)};
    });
    install(evaluator, "g_mkdir", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_mkdir");
        std::error_code error;
        fs::create_directories(evaluator.string_value(args[0]), error);
        return Values{Value::boolean(!error)};
    });
    install(evaluator, "g_rename", [&evaluator](const Values& args) {
        require_arity(args, 2, "g_rename");
        std::error_code error;
        fs::rename(evaluator.string_value(args[0]), evaluator.string_value(args[1]),
                   error);
        return Values{Value::boolean(!error)};
    });
    install(evaluator, "g_sha256", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_sha256");
        // This is the byte-string form used by the cache fingerprint.
        const std::string input = evaluator.string_value(args[0]);
        std::string result;
        tb_sha_t state;
        tb_sha_init(&state, 256);
        tb_sha_spak(&state,
                    reinterpret_cast<const tb_byte_t*>(input.data()),
                    static_cast<tb_size_t>(input.size()));
        unsigned char digest[32];
        tb_sha_exit(&state, digest, sizeof(digest));
        return Values{evaluator.string(digest_hex(digest, sizeof(digest)))};
    });
    install(evaluator, "g_sha256-by-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_sha256-by-file");
        auto digest = sha256_file(evaluator.string_value(args[0]));
        return Values{digest ? evaluator.string(*digest)
                             : Value::boolean(false)};
    });
}

} // namespace goldfish::runtime
