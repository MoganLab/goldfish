package("nlohmann_json")
    set_kind("library", {headeronly = true})
    set_homepage("https://github.com/nlohmann/json")
    set_description("JSON for Modern C++")
    set_license("MIT")

    set_sourcedir(path.join(os.scriptdir(), "../../../../3rdparty/nlohmann_json"))

    on_install(function (package)
        os.cp("include/nlohmann", package:installdir("include"))
        local cmake_dir = path.join(package:installdir("lib"), "cmake", "nlohmann_json")
        os.mkdir(cmake_dir)
        io.writefile(path.join(cmake_dir, "nlohmann_jsonConfig.cmake"), [[
if(NOT TARGET nlohmann_json::nlohmann_json)
    add_library(nlohmann_json::nlohmann_json INTERFACE IMPORTED)
    set_target_properties(nlohmann_json::nlohmann_json PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_LIST_DIR}/../../../include")
endif()
set(nlohmann_json_FOUND TRUE)
set(nlohmann_json_VERSION "3.11.3")
]])
        io.writefile(path.join(cmake_dir, "nlohmann_jsonConfigVersion.cmake"), [[
set(PACKAGE_VERSION "3.11.3")
if(PACKAGE_FIND_VERSION VERSION_LESS_EQUAL PACKAGE_VERSION)
    set(PACKAGE_VERSION_COMPATIBLE TRUE)
endif()
if(PACKAGE_FIND_VERSION STREQUAL PACKAGE_VERSION)
    set(PACKAGE_VERSION_EXACT TRUE)
endif()
]])
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
