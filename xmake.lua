set_version ("18.11.20")

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

local TBOX_VERSION = "1.8.0"
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
    add_requires("emscripten 3.1.56")
else
    add_requires("emscripten")
end
    set_toolchains("emcc@emscripten")
end

-- Keep the native runtime layer in one place.  Standalone tests use the core
-- set; bootstrap-capable targets add artifact/bootstrap on top of it.
local function add_native_runtime_sources()
    add_files("src/runtime/evaluator.cpp")
    add_files("src/runtime/core_evaluator.cpp")
    add_files("src/runtime/reader.cpp")
    add_files("src/runtime/migration_primitives.cpp")
    add_files("src/runtime/bootstrap_primitives.cpp")
    add_files("src/runtime/standard_primitives.cpp")
    add_files("src/runtime/platform_primitives.cpp")
    add_files("src/runtime/unicode_char.cpp")
    add_files("src/runtime/unicode_primitives.cpp")
    add_files("src/runtime/legacy_primitives.cpp")
    add_files("src/runtime/artifact.cpp")
end

local function add_native_bootstrap_sources()
    add_files("src/runtime/evaluator.cpp")
    add_files("src/runtime/core_evaluator.cpp")
    add_files("src/runtime/reader.cpp")
    add_files("src/runtime/bootstrap_compatibility.cpp")
    add_files("src/runtime/bootstrap_primitives.cpp")
    add_files("src/runtime/standard_primitives.cpp")
    add_files("src/runtime/platform_primitives.cpp")
    add_files("src/runtime/unicode_char.cpp")
    add_files("src/runtime/unicode_primitives.cpp")
    add_files("src/runtime/artifact.cpp")
    add_files("src/runtime/bootstrap.cpp")
end

local function add_goldfish_install_files()
    -- Scheme sources and tooling shipped with the native executable.
    add_installfiles("$(projectdir)/goldfish/(core/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(expander/kernel-combined.scm)", {prefixdir = "share/goldfish/expander"})
    add_installfiles("$(projectdir)/goldfish/(expander/bootstrap-prelude.scm)", {prefixdir = "share/goldfish/expander"})
    add_installfiles("$(projectdir)/goldfish/(expander/lib/*.scm)", {prefixdir = "share/goldfish/expander/lib"})
    add_installfiles("$(projectdir)/goldfish/(expander/tree-il.scm)", {prefixdir = "share/goldfish/expander"})
    add_installfiles("$(projectdir)/goldfish/(compiler/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(compiler.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(compiler/syntax-ir.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(scheme/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(srfi/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(liii/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(liii/path/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(guenchi/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/gfproject.scm", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/node-rules.json", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/(tools/**)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/(tests/**)", {prefixdir = "share/goldfish"})
end

target("lint-layer")
    set_kind("phony")
    on_build(function (target)
        os.exec("sh tools/lint-layer.sh")
    end)
target_end()

-- L3 expander kernel artifact: rebuild / reproduction guard.  Both run a
-- cold-cache from-artifact bootstrap (warm caches drift in gensym
-- numbering; see tools/build-kernel.sh and LAYER.md runtime-layer description).
-- set_default(false): maintenance targets, explicit invocation only --
-- otherwise a rebuilt goldfish binary drags them into plain `xmake b`,
-- and the two would race over the shared program cache.
target("kernel")
    set_kind("phony")
    set_default(false)
    add_deps("gf-native")
    on_build(function (target)
        os.exec("sh tools/build-kernel.sh")
    end)
target_end()

target("verify-kernel")
    set_kind("phony")
    set_default(false)
    add_deps("gf-native")
    on_build(function (target)
        os.exec("sh tools/verify-kernel.sh")
    end)
target_end()

target("native-source-bootstrap-test")
    set_kind("binary")
    set_targetdir("$(projectdir)/bin/")
    set_basename("native-source-bootstrap-test")
    set_languages("c++17")
    add_includedirs("src")
    add_files("tests/runtime/native-source-bootstrap-test.cpp")
    add_native_bootstrap_sources()
    add_packages("tbox")
target_end()

target("native-library-source-test")
    set_kind("binary")
    set_targetdir("$(projectdir)/bin/")
    set_basename("native-library-source-test")
    set_languages("c++17")
    add_includedirs("src")
    add_files("tests/runtime/native-library-source-test.cpp")
    add_native_bootstrap_sources()
    add_packages("tbox")
target_end()

target("native-dependency-test")
    set_kind("binary")
    set_targetdir("$(projectdir)/bin/")
    set_basename("native-dependency-test")
    set_languages("c++17")
    add_includedirs("src")
    add_files("tests/runtime/native-dependency-test.cpp")
    add_native_bootstrap_sources()
    add_packages("tbox")
target_end()

target("native-reader-test")
    set_kind("binary")
    set_targetdir("$(projectdir)/bin/")
    set_basename("native-reader-test")
    set_languages("c++17")
    add_includedirs("src")
    add_files("tests/runtime/reader-test.cpp")
    add_native_runtime_sources()
    add_packages("tbox")
target_end()

target("native-evaluator-test")
    set_kind("binary")
    set_targetdir("$(projectdir)/bin/")
    set_basename("native-evaluator-test")
    set_languages("c++17")
    add_includedirs("src")
    add_files("tests/runtime/evaluator-test.cpp")
    add_native_runtime_sources()
    add_packages("tbox")
target_end()

-- Vendored BDWGC conservative collector.  Only the native runtime links
-- it: every C++ allocation goes through GC_malloc, so ordinary stack
-- slots and containers root their values and collection needs no
-- per-frame root plumbing.  Host gf keeps the precise backend.
target("bdwgc")
    set_kind("static")
    set_languages("gnu99")
    add_includedirs("third_party/bdwgc/include")
    add_defines("ALL_INTERIOR_POINTERS", "NO_EXECUTE_PERMISSION",
                "GC_BUILTIN_ATOMIC")
    add_files(
        "third_party/bdwgc/allchblk.c",
        "third_party/bdwgc/alloc.c",
        "third_party/bdwgc/blacklst.c",
        "third_party/bdwgc/checksums.c",
        "third_party/bdwgc/dbg_mlc.c",
        "third_party/bdwgc/dyn_load.c",
        "third_party/bdwgc/finalize.c",
        "third_party/bdwgc/fnlz_mlc.c",
        "third_party/bdwgc/gc_dlopen.c",
        "third_party/bdwgc/gcj_mlc.c",
        "third_party/bdwgc/headers.c",
        "third_party/bdwgc/mach_dep.c",
        "third_party/bdwgc/malloc.c",
        "third_party/bdwgc/mallocx.c",
        "third_party/bdwgc/mark.c",
        "third_party/bdwgc/mark_rts.c",
        "third_party/bdwgc/misc.c",
        "third_party/bdwgc/new_hblk.c",
        "third_party/bdwgc/os_dep.c",
        "third_party/bdwgc/ptr_chck.c",
        "third_party/bdwgc/reclaim.c",
        "third_party/bdwgc/typd_mlc.c")
    add_syslinks("dl", "pthread")
target_end()

target("gf-native")
    set_kind("binary")
    set_default(true)
    set_targetdir("$(projectdir)/bin/")
    set_basename("gf")
    set_languages("c++17")
    add_includedirs("src")
    add_includedirs("third_party/bdwgc/include")
    add_files("src/runtime/native_main.cpp")
    add_files("src/runtime/gc_alloc.cpp")
    add_defines("GOLDFISH_HAVE_BDWGC")
    add_native_bootstrap_sources()
    add_deps("bdwgc")
    add_packages("tbox")
    add_goldfish_install_files()
target_end()

target("native-test")
    set_kind("phony")
    set_default(false)
    add_deps("gf-native", "native-reader-test")
    on_build(function (target)
        os.exec("bin/native-reader-test")
        os.exec("sh tools/test-native-cold-bootstrap.sh")
    end)
target_end()

includes("@builtin/xpack")

xpack ("goldfish")
    if is_plat("windows") then
        set_formats("zip")
    elseif is_plat("macosx") then
        set_formats("targz")
    else
        set_formats("deb", "rpm", "srpm")
    end
    set_author("Da Shen <da@liii.pro>")
    set_license("Apache-2.0")
    set_title("Goldfish Scheme")
    set_description("A Python-like Scheme Interpreter") 
    set_homepage("https://gitee.com/LiiiLabs/goldfish")
    add_targets ("gf-native")
    add_sourcefiles("(xmake/**)")
    add_sourcefiles("xmake.lua")
    add_sourcefiles("(src/**)")
    add_sourcefiles("(goldfish/**)")
    add_sourcefiles("(tests/**)")
    add_sourcefiles("(tools/**)")
    add_sourcefiles("(3rdparty/**)")
    add_sourcefiles("gfproject.scm")
    add_sourcefiles("node-rules.json")
    on_load(function (package)
        if package:with_source() then
            package:set("basename", "goldfish-scheme-src-v$(version)")
        elseif is_plat("windows") then
            package:set("basename", "goldfish-scheme-$(arch)-v$(version)-win")
        elseif is_plat("macosx") then
            package:set("basename", "goldfish-scheme-$(arch)-v$(version)-darwin")
        else
            package:set("basename", "goldfish-scheme-$(arch)-v$(version)")
        end
    end)
