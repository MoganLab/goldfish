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
#include <thread>
#include <utility>

#include <tbox/hash/md5.h>
#include <tbox/hash/sha.h>

#if !defined(_WIN32)
#include <pwd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
extern char** environ;
#endif

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif

namespace goldfish::runtime {

namespace {

namespace fs = std::filesystem;

// Set from main() before anything evaluates (command-line).
std::vector<std::string> g_native_command_line;

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

std::optional<std::string> sha1_file(const std::string& path) {
    tb_sha_t state;
    tb_sha_init(&state, 160);
    return digest_file(
        path,
        [&state](const unsigned char* data, std::size_t size) {
            tb_sha_spak(&state, data, static_cast<tb_size_t>(size));
        },
        [&state](unsigned char* digest, std::size_t size) {
            tb_sha_exit(&state, digest, static_cast<tb_size_t>(size));
        },
        20);
}

// Real executable path: g_executable feeds the cache fingerprint and is
// what the test tool spawns for its workers, so it must not be a stub.
std::string executable_path() {
#if defined(__linux__)
    std::error_code error;
    auto path = fs::read_symlink("/proc/self/exe", error);
    if (!error) return path.string();
#elif defined(__APPLE__)
    char buffer[4096];
    uint32_t size = sizeof(buffer);
    if (_NSGetExecutablePath(buffer, &size) == 0) {
        std::error_code error;
        auto canonical = fs::weakly_canonical(buffer, error);
        return error ? std::string(buffer) : canonical.string();
    }
#endif
    return {};
}

} // namespace

void set_native_command_line(int argc, char** argv) {
    g_native_command_line.clear();
    for (int i = 0; i < argc; ++i)
        g_native_command_line.emplace_back(argv[i]);
}

void install_platform_primitives(Evaluator& evaluator) {
    install(evaluator, "getenv", [&evaluator](const Values& args) {
        require_arity(args, 1, "getenv");
        const char* value = std::getenv(evaluator.string_value(args[0]).c_str());
        return Values{value ? evaluator.string(value) : Value::boolean(false)};
    });
    install(evaluator, "auto-compile-enabled?", [](const Values& args) {
        require_arity(args, 0, "auto-compile-enabled?");
        const char* value = std::getenv("GOLDFISH_AUTO_COMPILE");
        if (!value) return Values{Value::boolean(true)};
        const std::string setting(value);
        return Values{Value::boolean(setting != "0" && setting != "no" &&
                                     setting != "false" && setting != "off")};
    });
    install(evaluator, "g_path-getsize", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_path-getsize");
#if !defined(_WIN32)
        struct stat info {};
        const auto path = evaluator.string_value(args[0]);
        if (::stat(path.c_str(), &info) == 0) {
            return Values{Value::integer(static_cast<std::int64_t>(info.st_size))};
        }
#endif
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
        // Keep this view synchronized with Scheme's mutable *load-path*.
        // Library introspection and the loader both consult this primitive;
        // a startup-only snapshot misses paths added by a running program.
        try {
            return Values{evaluator.global_environment()->lookup(
                evaluator.symbol("*load-path*"))};
        } catch (const std::runtime_error&) {
            // During early bootstrap *load-path* may not exist yet.
        }
        return Values{evaluator.list({evaluator.string("."),
                                      evaluator.string("goldfish")})};
    });
    install(evaluator, "g_executable", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_executable");
        const std::string path = executable_path();
        return Values{path.empty() ? evaluator.string("gf")
                                   : evaluator.string(path)};
    });
    install(evaluator, "g_getpid", [](const Values& args) {
        require_arity(args, 0, "g_getpid");
#if defined(_WIN32)
        return Values{Value::integer(0)};
#else
        return Values{Value::integer(static_cast<std::int64_t>(::getpid()))};
#endif
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
        // Full argv, program first -- what (scheme process-context)'s
        // command-line and (liii sys)'s argv hand to argparse (which drops
        // the program itself).  The old stub returned ("goldfish") only.
        std::vector<Value> words;
        for (const std::string& word : g_native_command_line)
            words.push_back(evaluator.string(word));
        if (words.empty()) words.push_back(evaluator.string("gf"));
        return Values{evaluator.list(words)};
    });
    // Kernel-table name: (scheme process-context) defines its own for
    // importers, but libraries that only import (goldfish) -- the test
    // worker does -- resolve the bare reference against the global.
    install(evaluator, "command-line", [&evaluator](const Values& args) {
        require_arity(args, 0, "command-line");
        std::vector<Value> words;
        for (const std::string& word : g_native_command_line)
            words.push_back(evaluator.string(word));
        if (words.empty()) words.push_back(evaluator.string("gf"));
        return Values{evaluator.list(words)};
    });
    install(evaluator, "version", [&evaluator](const Values& args) {
        require_arity(args, 0, "version");
        // Keep in sync with the version reported by the native executable.
        return Values{evaluator.string("18.11.20")};
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

    // --- platform surface needed by (liii os/path/sys) and the project
    // --- tool chain; semantics mirror the host's liii_os.cpp wrappers.

    install(evaluator, "g_getcwd", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_getcwd");
        std::error_code error;
        auto path = fs::current_path(error);
        return Values{error ? evaluator.string("") : evaluator.string(path.string())};
    });
    install(evaluator, "g_chdir", [](const Values& args) {
        require_arity(args, 1, "g_chdir");
        std::error_code error;
        fs::current_path(args[0].as_object<StringObject>()->value, error);
        return Values{Value::boolean(!error)};
    });
    install(evaluator, "g_isfile", [](const Values& args) {
        require_arity(args, 1, "g_isfile");
        std::error_code error;
        bool result = fs::is_regular_file(
            args[0].as_object<StringObject>()->value, error);
        return Values{Value::boolean(!error && result)};
    });
    install(evaluator, "g_isdir", [](const Values& args) {
        require_arity(args, 1, "g_isdir");
        std::error_code error;
        bool result = fs::is_directory(
            args[0].as_object<StringObject>()->value, error);
        return Values{Value::boolean(!error && result)};
    });
    install(evaluator, "g_access", [](const Values& args) {
        require_arity(args, 2, "g_access");
        const std::string path = args[0].as_object<StringObject>()->value;
        const std::int64_t mode = args[1].as_integer();
#if defined(_WIN32)
        std::error_code error;
        bool result = fs::exists(path, error);
        if (!error && mode != 0) result = fs::is_regular_file(path, error) && !error;
        return Values{Value::boolean(result)};
#else
        // 0 = exists, 1 = readable (the seed's permission check), else
        // writable -- matching how the Scheme callers use the host wrapper.
        int request = F_OK;
        if (mode == 1) request = R_OK;
        else if (mode != 0) request = W_OK;
        return Values{Value::boolean(::access(path.c_str(), request) == 0)};
#endif
    });
    install(evaluator, "g_remove-file", [](const Values& args) {
        require_arity(args, 1, "g_remove-file");
        std::error_code error;
        bool removed = fs::remove(args[0].as_object<StringObject>()->value, error);
        return Values{Value::boolean(!error && removed)};
    });
    install(evaluator, "g_rmdir", [](const Values& args) {
        require_arity(args, 1, "g_rmdir");
        std::error_code error;
        fs::remove(args[0].as_object<StringObject>()->value, error);
        return Values{Value::boolean(!error)};
    });
    install(evaluator, "g_setenv", [&evaluator](const Values& args) {
        require_arity(args, 2, "g_setenv");
        const std::string key = evaluator.string_value(args[0]);
        const std::string value = evaluator.string_value(args[1]);
#if defined(_WIN32)
        return Values{Value::boolean(_putenv_s(key.c_str(), value.c_str()) == 0)};
#else
        return Values{Value::boolean(::setenv(key.c_str(), value.c_str(), 1) == 0)};
#endif
    });
    install(evaluator, "g_unsetenv", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_unsetenv");
#if defined(_WIN32)
        return Values{Value::boolean(
            _putenv_s(evaluator.string_value(args[0]).c_str(), "") == 0)};
#else
        return Values{
            Value::boolean(::unsetenv(evaluator.string_value(args[0]).c_str()) == 0)};
#endif
    });
    install(evaluator, "g_getlogin", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_getlogin");
#if defined(_WIN32)
        return Values{evaluator.string("")};
#else
        struct passwd* entry = ::getpwuid(::getuid());
        return Values{entry ? evaluator.string(entry->pw_name)
                            : evaluator.string("")};
#endif
    });
    install(evaluator, "g_os-temp-dir", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_os-temp-dir");
        if (const char* tmp = std::getenv("TMPDIR"))
            if (*tmp) return Values{evaluator.string(tmp)};
        std::error_code error;
        auto path = fs::temp_directory_path(error);
        return Values{error ? evaluator.string("/tmp")
                            : evaluator.string(path.string())};
    });
    install(evaluator, "g_os-call", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_os-call");
        const std::string command = evaluator.string_value(args[0]);
        // Scheme callers expect a plain exit status (goldtest compares it
        // with 0 and with the codes its worker scripts echo), so normalize
        // the shell's wait status here.
        const int status = std::system(command.c_str());
#if defined(_WIN32)
        return Values{Value::integer(status)};
#else
        if (status == -1) return Values{Value::integer(-1)};
        if (WIFEXITED(status)) return Values{Value::integer(WEXITSTATUS(status))};
        if (WIFSIGNALED(status))
            return Values{Value::integer(128 + WTERMSIG(status))};
        return Values{Value::integer(-1)};
#endif
    });
    install(evaluator, "g_os-arch", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_os-arch");
#if defined(__x86_64__) || defined(_M_X64)
        return Values{evaluator.string("x86_64")};
#elif defined(__aarch64__) || defined(_M_ARM64)
        return Values{evaluator.string("aarch64")};
#elif defined(__i386__) || defined(_M_IX86)
        return Values{evaluator.string("x86")};
#elif defined(__arm__)
        return Values{evaluator.string("arm")};
#else
        return Values{evaluator.string("")};
#endif
    });
    install(evaluator, "g_os-type", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_os-type");
#if defined(__linux__)
        return Values{evaluator.string("Linux")};
#elif defined(__APPLE__)
        return Values{evaluator.string("Darwin")};
#elif defined(_WIN32)
        return Values{evaluator.string("Windows")};
#else
        return Values{evaluator.string("")};
#endif
    });
    install(evaluator, "g_sleep", [](const Values& args) {
        require_arity(args, 1, "g_sleep");
        // (liii time)'s sleep passes seconds.
        std::this_thread::sleep_for(std::chrono::seconds(args[0].as_integer()));
        return Values{Value::unspecified()};
    });
    install(evaluator, "g_which", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_which");
        const std::string name = evaluator.string_value(args[0]);
        const char* path = std::getenv("PATH");
        if (!path || name.empty() || name.find('/') != std::string::npos)
            return Values{Value::boolean(false)};
        const std::string raw(path);
        std::size_t start = 0;
        while (start <= raw.size()) {
            const std::size_t end = raw.find(':', start);
            const std::string dir =
                raw.substr(start, end == std::string::npos ? end : end - start);
            if (!dir.empty()) {
                std::error_code error;
                const fs::path candidate = fs::path(dir) / name;
                if (fs::is_regular_file(candidate, error) && !error) {
                    auto permissions = fs::status(candidate, error).permissions();
                    if (!error && (permissions & fs::perms::owner_exec) !=
                                      fs::perms::none)
                        return Values{evaluator.string(candidate.string())};
                }
            }
            if (end == std::string::npos) break;
            start = end + 1;
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "g_goldfish-library", [&evaluator](const Values& args) {
        require_arity(args, 0, "g_goldfish-library");
        // Same layout as the host: the library root sits next to the bin
        // directory holding this executable (repo checkout: <root>/bin/gf).
        const std::string exe = executable_path();
        if (exe.empty()) return Values{evaluator.string(".")};
        std::error_code error;
        auto root = fs::path(exe).parent_path().parent_path();
        if (!fs::is_directory(root / "goldfish", error))
            return Values{evaluator.string(".")};
        return Values{evaluator.string(root.string())};
    });

    // --- file/text helpers used by (liii path) and the tool chain.

    install(evaluator, "g_path-read-text", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_path-read-text");
        std::ifstream input(evaluator.string_value(args[0]), std::ios::binary);
        if (!input) return Values{Value::boolean(false)};
        std::string content((std::istreambuf_iterator<char>(input)),
                            std::istreambuf_iterator<char>());
        std::string normalized;
        normalized.reserve(content.size());
        for (std::size_t i = 0; i < content.size(); ++i) {
            if (content[i] == '\r') {
                if (i + 1 < content.size() && content[i + 1] == '\n') ++i;
                normalized.push_back('\n');
            } else {
                normalized.push_back(content[i]);
            }
        }
        return Values{evaluator.string(normalized)};
    });
    install(evaluator, "g_path-read-bytes", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_path-read-bytes");
        std::ifstream input(evaluator.string_value(args[0]), std::ios::binary);
        if (!input) return Values{Value::boolean(false)};
        std::string bytes((std::istreambuf_iterator<char>(input)),
                          std::istreambuf_iterator<char>());
        return Values{Value::object(
            evaluator.heap().make<BytevectorObject>(std::move(bytes)))};
    });
    install(evaluator, "g_path-write-text", [&evaluator](const Values& args) {
        require_arity(args, 2, "g_path-write-text");
        const std::string content = evaluator.string_value(args[1]);
        std::ofstream output(evaluator.string_value(args[0]),
                             std::ios::binary | std::ios::trunc);
        if (!output) return Values{Value::integer(-1)};
        output.write(content.data(),
                     static_cast<std::streamsize>(content.size()));
        if (!output) return Values{Value::integer(-1)};
        return Values{Value::integer(static_cast<std::int64_t>(content.size()))};
    });
    install(evaluator, "g_path-write-bytes", [&evaluator](const Values& args) {
        require_arity(args, 2, "g_path-write-bytes");
        Value data = args[1];
        if (!data.is_object() ||
            data.as_object()->type() != ObjectType::Bytevector)
            throw std::runtime_error("g_path-write-bytes expects a bytevector");
        std::ofstream output(evaluator.string_value(args[0]),
                             std::ios::binary | std::ios::trunc);
        if (!output) return Values{Value::integer(-1)};
        const std::string& bytes = data.as_object<BytevectorObject>()->bytes;
        output.write(bytes.data(), static_cast<std::streamsize>(bytes.size()));
        if (!output) return Values{Value::integer(-1)};
        return Values{Value::integer(static_cast<std::int64_t>(bytes.size()))};
    });
    install(evaluator, "g_path-append-text", [&evaluator](const Values& args) {
        require_arity(args, 2, "g_path-append-text");
        const std::string content = evaluator.string_value(args[1]);
        std::ofstream output(evaluator.string_value(args[0]),
                             std::ios::binary | std::ios::app);
        if (!output) return Values{Value::integer(-1)};
        output.write(content.data(),
                     static_cast<std::streamsize>(content.size()));
        if (!output) return Values{Value::integer(-1)};
        return Values{Value::integer(static_cast<std::int64_t>(content.size()))};
    });
    install(evaluator, "g_path-touch", [](const Values& args) {
        require_arity(args, 1, "g_path-touch");
        const fs::path target(args[0].as_object<StringObject>()->value);
        std::error_code error;
        if (fs::exists(target, error)) {
            fs::last_write_time(target, fs::file_time_type::clock::now(), error);
            return Values{Value::boolean(!error)};
        }
        error.clear();
        std::ofstream create(target, std::ios::binary);
        return Values{Value::boolean(static_cast<bool>(create))};
    });
    install(evaluator, "g_path-copy", [](const Values& args) {
        require_arity(args, 2, "g_path-copy");
        std::error_code error;
        fs::copy_file(args[0].as_object<StringObject>()->value,
                      args[1].as_object<StringObject>()->value,
                      fs::copy_options::overwrite_existing, error);
        return Values{Value::boolean(!error)};
    });
    install(evaluator, "g_delete-file", [](const Values& args) {
        require_arity(args, 1, "g_delete-file");
        std::error_code error;
        bool removed = fs::remove(args[0].as_object<StringObject>()->value, error);
        return Values{Value::boolean(!error && removed)};
    });
    install(evaluator, "g_md5", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_md5");
        const std::string input = evaluator.string_value(args[0]);
        tb_md5_t state;
        tb_md5_init(&state, 0);
        tb_md5_spak(&state, reinterpret_cast<const tb_byte_t*>(input.data()),
                    static_cast<tb_size_t>(input.size()));
        unsigned char digest[16];
        tb_md5_exit(&state, digest, sizeof(digest));
        return Values{evaluator.string(digest_hex(digest, sizeof(digest)))};
    });
    install(evaluator, "g_sha1", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_sha1");
        const std::string input = evaluator.string_value(args[0]);
        tb_sha_t state;
        tb_sha_init(&state, 160);
        tb_sha_spak(&state, reinterpret_cast<const tb_byte_t*>(input.data()),
                    static_cast<tb_size_t>(input.size()));
        unsigned char digest[20];
        tb_sha_exit(&state, digest, sizeof(digest));
        return Values{evaluator.string(digest_hex(digest, sizeof(digest)))};
    });
    install(evaluator, "g_sha1-by-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "g_sha1-by-file");
        auto digest = sha1_file(evaluator.string_value(args[0]));
        return Values{digest ? evaluator.string(*digest)
                             : Value::boolean(false)};
    });
}

} // namespace goldfish::runtime
