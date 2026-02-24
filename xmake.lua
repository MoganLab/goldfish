set_version ("17.11.26")

-- mode
set_allowedmodes("releasedbg", "release", "debug", "profile")
add_rules("mode.releasedbg", "mode.release", "mode.debug", "mode.profile")

-- plat
set_allowedplats("linux", "macosx", "windows", "wasm")

-- proj
set_project("Goldfish Scheme")

-- repo
add_repositories("goldfish-repo xmake")

option("tbox")
    set_description("Use tbox installed via apt")
    set_default(false)
    set_values(false, true)
option_end()

option("repl")
    set_description("Enable REPL (isocline) support")
    set_default(false) -- repl-anchor
    set_values(false, true)
option_end()

option("system-deps")
    set_description("Use system dependences")
    set_default(false)
    set_values(false, true)
option_end()
local system = has_config("system-deps")

option("pin-deps")
    set_description("Pin dependences version")
    set_default(true)
    set_values(false, true)
option_end()

option("http")
    set_description("Enable http")
    set_default(true)
    set_values(false, true)
option_end()

option("gmp")
    set_description("Enable GMP support for S7")
    set_default(false)
    set_values(false, true)
option_end()

if has_config("http") then
    add_requires("cpr")
end

if has_config("gmp") then
    add_requires("gmp")
end

-- S7 is now included as source files in src/ directory

local TBOX_VERSION = "1.7.7"
if has_config("tbox") then
    add_requires("apt::libtbox-dev", {alias="tbox"})
else
    tbox_configs = {hash=true, ["force-utf8"]=true}
    if has_config("pin-deps") then
        add_requires("tbox " .. TBOX_VERSION, {system=system, configs=tbox_configs})
    else
        add_requires("tbox", {system=system, configs=tbox_configs})
    end
end

if is_plat("wasm") then
if has_config("pin-deps") then
    add_requires("emscripten 3.1.55")
else
    add_requires("emscripten")
end
    set_toolchains("emcc@emscripten")
end

local IC_VERSION = "v1.0.9"
if has_config("pin-deps") then
    add_requires("isocline " .. IC_VERSION, {system=system})
else
    add_requires("isocline", {system=system})
end

-- local header only dependency, no need to (un)pin version
add_requires("argh v1.3.2")

target ("goldfish") do
    set_languages("c++17")
    set_targetdir("$(projectdir)/bin/")
    if is_plat("linux") then
        add_syslinks("stdc++")
    end
    if is_plat("wasm") then
        -- preload goldfish stdlib in `bin/goldfish.data`
        add_ldflags("--preload-file goldfish@/goldfish")
    end
    add_files ("src/goldfish.cpp")
    add_files ("src/s7.c", {languages = "c11"})
    add_files ("src/s7_liii_bitwise.c", {languages = "c11"})
    add_files ("src/s7_scheme_inexact.c", {languages = "c11"})
    add_files ("src/s7_scheme_base.c", {languages = "c11"})
    add_packages("tbox")
    add_packages("argh")
    add_packages("cpr")

    -- S7 configuration from original 3rdparty/s7/xmake.lua
    add_defines("WITH_SYSTEM_EXTRAS=0")
    if not is_plat("wasm") then
        add_defines("HAVE_OVERFLOW_CHECKS=0")
    end
    add_defines("WITH_WARNINGS")
    add_defines("WITH_R7RS=1")
    if is_mode("debug") then
        add_defines("S7_DEBUGGING")
    end
    add_options("gmp")
    if has_config("gmp") then
        add_defines("WITH_GMP")
        add_packages("gmp")
    end

    -- Windows-specific configuration from original 3rdparty/s7/xmake.lua
    if is_plat("windows") then
        set_optimize("faster")
        add_cxxflags("/fp:precise")
    end

    -- only enable REPL if repl option is enabled
    if has_config("repl") then
        add_packages("isocline")
        add_defines("GOLDFISH_WITH_REPL")
    end

    add_installfiles("$(projectdir)/goldfish/(scheme/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(srfi/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(liii/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(guenchi/*.scm)", {prefixdir = "share/goldfish"})
end

if is_plat("wasm") then
target("goldfish_repl_wasm")
    set_kind("binary")
    set_languages("c++17")
    set_targetdir("$(projectdir)/repl/")
    add_files("src/goldfish_repl.cpp")
    add_packages("tbox", "argh")
    add_defines("GOLDFISH_ENABLE_REPL")

    -- S7 configuration from original 3rdparty/s7/xmake.lua
    add_defines("WITH_SYSTEM_EXTRAS=0")
    -- WASM platform doesn't have HAVE_OVERFLOW_CHECKS=0
    add_defines("WITH_WARNINGS")
    add_defines("WITH_R7RS=1")
    if is_mode("debug") then
        add_defines("S7_DEBUGGING")
    end
    add_options("gmp")
    if has_config("gmp") then
        add_defines("WITH_GMP")
        add_packages("gmp")
    end

    add_ldflags("--preload-file goldfish@/goldfish")
    -- 导出 REPL 相关函数
    add_ldflags("-sEXPORTED_FUNCTIONS=['_eval_string','_get_out','_get_err','_malloc','_free']", {force = true})
    add_ldflags("-sEXPORTED_RUNTIME_METHODS=['UTF8ToString','allocateUTF8']", {force = true})
    add_ldflags("-sINITIAL_MEMORY=134217728", {force = true})
    add_ldflags("-sALLOW_MEMORY_GROWTH=1", {force = true})
    add_ldflags("-sASSERTIONS=1", {force = true})
    -- 生成 js glue code
    set_extension(".js")
end

includes("@builtin/xpack")

xpack ("goldfish")
    set_formats("deb", "rpm", "srpm")
    set_author("Da Shen <da@liii.pro>")
    set_license("Apache-2.0")
    set_title("Goldfish Scheme")
    set_description("A Python-like Scheme Interpreter") 
    set_homepage("https://gitee.com/LiiiLabs/goldfish")
    add_targets ("goldfish")
    add_sourcefiles("(xmake/**)")
    add_sourcefiles("xmake.lua")
    add_sourcefiles("(src/**)")
    add_sourcefiles("(goldfish/**)")
    add_sourcefiles("(3rdparty/**)")
    on_load(function (package)
        if package:with_source() then
            package:set("basename", "goldfish-$(plat)-src-v$(version)")
        else
            package:set("basename", "goldfish-$(plat)-$(arch)-v$(version)")
        end
    end)
