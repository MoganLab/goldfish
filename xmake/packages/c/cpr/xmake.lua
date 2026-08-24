-- 本地覆盖 xmake-repo 的 cpr 配方（定制方式移植自 mogan，上游见
-- ~/git/xmake-repo/packages/c/cpr/xmake.lua），差异：
-- 1. 上游 ssl=true 时给 libcurl 加 {libssh2=true, zlib=true} 并额外依赖 libssh2，
--    会产生第二个带额外 configs 的 libcurl 实例，导致两份静态 curl 符号冲突。
-- 2. goldfish 只需要 HTTPS，不需要 libssh2/zlib，统一依赖不带额外 configs 的
--    libcurl（本仓库 3rdparty/curl-8.21.0 源码构建，见 xmake/packages/l/libcurl），
--    避免引入系统 libcurl 与静态 openssl 混用导致的问题。
-- 3. 源码使用仓库内 3rdparty/cpr（set_sourcedir），不从网络下载。
package("cpr")

    set_homepage("https://docs.libcpr.org/")
    set_description("C++ Requests is a simple wrapper around libcurl inspired by the excellent Python Requests project.")
    set_license("MIT")

    set_sourcedir(path.join(os.scriptdir(), "../../../../3rdparty/cpr"))

    add_configs("ssl", {description = "Enable SSL.", default = false, type = "boolean"})

    add_deps("cmake")
    if is_plat("mingw", "linux") then
        add_syslinks("pthread")
    end
    add_links("cpr")

    on_load(function (package)
        -- TLS 能力由 libcurl 自身（openssl/openssl3）提供，cpr 只是透传 CPR_ENABLE_SSL；
        -- 不带额外 configs，保证 libcurl 解析为同一实例
        package:add("deps", "libcurl")
    end)

    on_install("!wasm and !bsd", function (package)
        io.replace("CMakeLists.txt", "-Werror", "", {plain = true})
        if package:is_plat("windows") or (package:is_plat("android") and is_subhost("windows")) then
            -- fix find_package issue on windows
            io.replace("CMakeLists.txt", "find_package%(CURL COMPONENTS .-%)", "find_package(CURL)")
        end

        local configs = {"-DCPR_BUILD_TESTS=OFF",
                         "-DCPR_FORCE_USE_SYSTEM_CURL=ON",
                         "-DCPR_USE_SYSTEM_CURL=ON"}
        table.insert(configs, "-DCMAKE_BUILD_TYPE=" .. (package:debug() and "Debug" or "Release"))
        table.insert(configs, "-DBUILD_SHARED_LIBS=" .. (package:config("shared") and "ON" or "OFF"))
        table.insert(configs, "-DCPR_ENABLE_SSL=" .. (package:config("ssl") and "ON" or "OFF"))

        local opt = {}
        opt.packagedeps = {"libcurl"}
        if package:is_plat("windows") and package:has_tool("cxx", "cl", "clang_cl") then
            opt.cxflags = {"/EHsc"}
        end
        if package:config("shared") and package:is_plat("macosx") then
            opt.shflags = {"-framework", "CoreFoundation", "-framework", "Security", "-framework", "SystemConfiguration"}
        end
        import("package.tools.cmake").install(package, configs, opt)
    end)

    on_test(function (package)
        assert(package:check_cxxsnippets({test = [[
            #include <cassert>
            #include <cpr/cpr.h>
            static void test() {
                cpr::Response r = cpr::Get(cpr::Url{"https://xmake.io"});
                assert(r.status_code == 200);
            }
        ]]}, {configs = {languages = "c++17"}}))
    end)
