package("nlohmann_json")
    set_kind("library", {headeronly = true})
    set_homepage("https://github.com/nlohmann/json")
    set_description("JSON for Modern C++")
    set_license("MIT")

    add_urls("https://github.com/nlohmann/json.git")
    add_versions("v3.11.3", "v3.11.3")

    on_install(function (package)
        os.cp("single_include/nlohmann", package:installdir("include"))
    end)

    on_test(function (package)
        assert(package:check_cxxsnippets({test = [[
            #include <nlohmann/json.hpp>

            int test() {
                nlohmann::json j = {{"ok", true}, {"value", 7}};
                return (j["ok"] == true && j["value"] == 7) ? 0 : 1;
            }
        ]]}, {configs = {languages = "c++11"}}))
    end)
