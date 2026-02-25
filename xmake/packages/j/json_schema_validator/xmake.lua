package("json_schema_validator")
    set_homepage("https://github.com/pboettch/json-schema-validator")
    set_description("JSON schema validator for nlohmann::json")
    set_license("MIT")

    set_sourcedir(path.join(os.scriptdir(), "../../../../3rdparty/json-schema-validator"))

    add_deps("cmake", "nlohmann_json")
    add_links("nlohmann_json_schema_validator")

    on_install("linux", "macosx", "windows", "mingw@windows", function (package)
        local nlohmann_json = package:dep("nlohmann_json")
        local nlohmann_json_cmake_dir = path.join(nlohmann_json:installdir("lib"), "cmake", "nlohmann_json")
        local configs = {
            "-DJSON_VALIDATOR_BUILD_TESTS=OFF",
            "-DJSON_VALIDATOR_BUILD_EXAMPLES=OFF",
            "-DJSON_VALIDATOR_INSTALL=ON",
            "-DJSON_VALIDATOR_SHARED_LIBS=" .. (package:config("shared") and "ON" or "OFF"),
            "-Dnlohmann_json_DIR=" .. nlohmann_json_cmake_dir,
            "-DCMAKE_PREFIX_PATH=" .. nlohmann_json:installdir()
        }
        table.insert(configs, "-DCMAKE_BUILD_TYPE=" .. (package:debug() and "Debug" or "Release"))
        import("package.tools.cmake").install(package, configs, {packagedeps = {"nlohmann_json"}})
    end)

    on_test(function (package)
        assert(package:check_cxxsnippets({test = [[
            #include <nlohmann/json-schema.hpp>
            #include <nlohmann/json.hpp>

            int test() {
                nlohmann::json_schema::json_validator validator;
                nlohmann::json instance = nlohmann::json::object();
                (void)instance;
                return 0;
            }
        ]]}, {configs = {languages = "c++17"}}))
    end)
