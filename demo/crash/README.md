# demo/crash — 让 bin/gf 崩溃的 Scheme 代码片段

每个文件都是一条可以让 `bin/gf` 进程异常终止的最小片段。验证方式：

```bash
bin/gf demo/crash/<文件名>; echo "exit=$?"
# 段错误:   exit=139 (SIGSEGV)
# 已中止:   exit=134 (SIGABRT, C++ 未捕获异常)
```

每个片段均已复现验证稳定崩溃（第三轮扫描的 30 个新片段为逐个复跑验证）。

## 关键发现：字符参数是万能触发器

对一切缺少类型检查的 `s7_string()` 入口，**字符（如 `#\a`）比整数可靠得多**：
s7 的字符是标记立即数，`s7_string` 底层 `(T_Str(p))->object.string.svalue`
（`s7_internal.h`，`T_Str(P) = P`）会把立即数当 cell 指针解引用低地址，
必然 SIGSEGV；而整数是堆 cell，其垃圾值经常碰巧可读，只是静默返回错误结果。

## 公开 API 级崩溃（最严重，包装层裸透传）

| 文件 | 触发代码 | 信号 | 根因 |
|---|---|---|---|
| p02-hashlib-sha256-char.scm | `(import (liii hashlib)) (sha256 #\a)` | SIGSEGV | 同上 |
| p03-os-unsetenv-char.scm | `(import (liii os)) (unsetenv #\a)` | SIGSEGV | `os.scm` 的 `unsetenv` 直接透传 `g_unsetenv` |

注：`(liii hashlib)` 的 `sha1`/`sha256`/`md5-by-file`/`sha1-by-file`/`sha256-by-file`
与 `md5` 同构，均裸透传。

## C 层入口（g_*）崩溃 — os 模块（liii_os.cpp）

| 文件 | 触发代码 | 信号 |
|---|---|---|
| c13-g_unsetenv-char.scm | `(g_unsetenv #\a)` | SIGSEGV |
| c14-g_mkdir-char.scm | `(g_mkdir #\a)` | SIGSEGV |
| c15-g_rmdir-char.scm | `(g_rmdir #\a)` | SIGSEGV |
| c16-g_remove-file-char.scm | `(g_remove-file #\a)` | SIGSEGV |
| c17-g_chdir-char.scm | `(g_chdir #\a)` | SIGSEGV |
| c18-g_access-char.scm | `(g_access #\a 0)` | SIGSEGV |
| c19-g_setenv-char.scm | `(g_setenv #\a "v")` | SIGSEGV |

## C 层入口 — path 模块（liii_path.cpp）

| 文件 | 触发代码 | 信号 |
|---|---|---|
| c20-g_isdir-char.scm | `(g_isdir #\a)` | SIGSEGV |
| c21-g_isfile-char.scm | `(g_isfile #\a)` | SIGSEGV |
| c22-g_path-getsize-char.scm | `(g_path-getsize #\a)` | SIGSEGV |
| c23-g_path-read-text-char.scm | `(g_path-read-text #\a)` | SIGSEGV |
| c24-g_path-read-bytes-char.scm | `(g_path-read-bytes #\a)` | SIGSEGV |
| c25-g_path-copy-char.scm | `(g_path-copy #\a "x")` | SIGSEGV |

## C 层入口 — hashlib 模块（liii_hashlib.cpp）

| 文件 | 触发代码 | 信号 |
|---|---|---|
| c27-g_md5-by-file-char.scm | `(g_md5-by-file #\a)` | SIGSEGV |
| c28-g_sha1-char.scm | `(g_sha1 #\a)` | SIGSEGV |
| c29-g_sha1-by-file-char.scm | `(g_sha1-by-file #\a)` | SIGSEGV |
| c30-g_sha256-char.scm | `(g_sha256 #\a)` | SIGSEGV |
| c31-g_sha256-by-file-char.scm | `(g_sha256-by-file #\a)` | SIGSEGV |

## C 层入口 — http 模块（liii_http.cpp）

| 文件 | 触发代码 | 信号 | 根因 |
|---|---|---|---|
| c32-g_http-head-integer.scm | `(g_http-head 42)` | SIGABRT | url 未检查，垃圾/空指针进 `cpr::Url`（std::string） |
| c33-g_http-get-url-integer.scm | `(g_http-get 42 '() '() '() #f)` | SIGABRT | 同上 |
| c34-g_http-get-params-badpair.scm | `(g_http-get "http://x/" '((1 . 2)) '() '() #f)` | SIGABRT | `to_cpr_parameters` 对 pair 的 key/value 都不检查，整数→空指针→`std::logic_error` |
| c35-g_http-get-params-nonpair.scm | `(g_http-get "http://x/" '(42) '() '() #f)` | SIGSEGV | 列表项不是 pair 时 `s7_car` 无检查解引用 |
| c36-g_http-get-headers-badpair.scm | `(g_http-get "http://x/" '() '((1 . 2)) '() #f)` | SIGABRT | `to_cpr_headers` 同 c34 |
| c37-g_http-get-proxy-badpair.scm | `(g_http-get "http://x/" '() '() '((1 . 2)) #f)` | SIGABRT | `to_cpr_proxies` 同 c34 |
| c38-g_http-post-url-integer.scm | `(g_http-post 42 "b" ...)` | SIGSEGV | url 未检查 |
| c39-g_http-post-body-integer.scm | `(g_http-post "http://x/" 42 ...)` | SIGSEGV | body 未检查 |

注：`(liii http)` 公开包装有完整类型检查（`http-require-string` 等），
http 崩溃仅能从根环境 `g_http-*` 直接触发。

## 历史条目（已修复并移除）

- `h01-os-call-public-integer.scm`（os-call 传入非字符串段错误）
  已在 devel/0142.md 修复。
- `h02-which-public-integer.scm`（which 传入非字符串 abort）
  已在 devel/0143.md 修复，错误类型统一为 `(liii error)` 的 type-error。
- `c09-g_rename-integer.scm`（g_rename 传入非字符串段错误）
  已在 devel/0144.md 修复。
- `c11-g_listdir-integer.scm`（g_listdir 传入非字符串 abort）
  已在 devel/0145.md 修复。
- `c26-g_md5-char.scm`（g_md5 传入非字符串段错误，同时修复公开接口 `md5` 的 `p01-hashlib-md5-char.scm`）
  已在 devel/0147.md 修复。

## 仍然有效的旧发现

| 文件 | 触发代码 | 信号 | 根因 |
|---|---|---|---|
| f03-base64-encode-oob-length.scm | `(g_bytevector-base64-encode (make-bytevector 4 65) 1073741824)` | SIGSEGV | 长度参数不校验，编码循环越界读取（修复见 devel/0146.md，待合并） |

## 涉及的共同根因

1. **C 胶水层不检查参数类型**：`liii_os.cpp`、`liii_path.cpp`、`liii_hashlib.cpp`、
   `liii_http.cpp` 及 `goldfish.hpp` 里的 `f_*` 函数大量直接调用
   `s7_string(s7_car(args))`。`s7_string` 对非字符串对象等同于把对象强转为
   字符串 cell 解引用：字符（立即数）必崩；整数等堆对象视垃圾指针落点，
   `std::string`/`wordexp`/`std::filesystem` 等立即 strlen 的路径稳定崩溃，
   `tb_file_info` 等宽松路径静默返回错误值。
2. **http 辅助函数对 alist 结构不做检查**：`to_cpr_parameters/headers/proxies`
   对列表项不检查是否 pair、pair 的两端不检查是否字符串。
3. **长度/容量参数不校验**：base64 编解码的第二参数与实际 bytevector 长度无关。
4. **Scheme 包装层防护不一致**：`hashlib.scm` 全部裸透传、`os.scm` 的
   `unsetenv` 裸透传；而 `http.scm`、`rename`/`listdir`/`mkdir` 等包装层有检查。

## 测试过但不会崩溃的方面（s7 核心防护完善）

- s7 自带 reader 对怪异 token（`#)` `#|` `#u8(300)` 等）全部干净报错
- 循环结构：`write`/`length`/`equal?` 都有环检测
- 深嵌套/深递归：s7 有栈保护；`(liii json)` 解析器有 10000 层深度限制；
  `(liii njson)` 底层 nlohmann::json 为迭代解析
- `vector-ref`/`string-ref` 越界、`make-vector -5`、`(string->number "1" 1)` 等均为干净报错
- `(liii json)` 的 C 层入口（`g_string->json`/`g_json-ref` 等）有类型检查
- `(liii base64)`、`(liii sort)`、`(liii string-cursor)`、`(liii subprocess)` 的
  C 层入口有 `s7_is_string`/`s7_is_integer` 检查
- `(liii os)` 公开包装的 `putenv`/`access`/`mkdir`/`rmdir`/`system` 有检查
