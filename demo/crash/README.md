# demo/crash — 让 bin/gf 崩溃的 Scheme 代码片段

每个文件都是一条可以让 `bin/gf`（Goldfish Scheme v18.11.30, s7 11.5）进程
异常终止的最小片段。验证方式：

```bash
bin/gf demo/crash/<文件名>; echo "exit=$?"
# 段错误:   exit=139 (SIGSEGV)
# 已中止:   exit=134 (SIGABRT, C++ 未捕获异常)
```

每个片段均已 3 次重复验证稳定复现。

历史条目：
- `h01-os-call-public-integer.scm`（os-call 传入非字符串段错误）
  已在 devel/0142.md 修复，修复验证完成后移除。
- `h02-which-public-integer.scm`（which 传入非字符串 abort）
  已在 devel/0143.md 修复，修复验证完成后移除。
- `c09-g_rename-integer.scm`（g_rename 传入非字符串段错误）
  已在 devel/0144.md 修复，修复验证完成后移除。
- `c11-g_listdir-integer.scm`（g_listdir 传入非字符串 abort）
  已在 devel/0145.md 修复，修复验证完成后移除。

## 已确认的崩溃点

| 文件 | 触发代码 | 信号 | 根因 |
|---|---|---|---|
| f03-base64-encode-oob-length.scm | `(g_bytevector-base64-encode (make-bytevector 4 65) 1073741824)` | SIGSEGV | `liii_base64.cpp` 的长度参数不校验是否超过 bytevector 实际大小，编码循环按声明长度越界读取 1GB |

## 涉及的共同根因

1. **C 胶水层不检查参数类型**：`liii_os.cpp`、`liii_path.cpp`、`liii_hashlib.cpp`
   以及 `goldfish.hpp` 里的 `f_*` 函数大量直接调用 `s7_string(s7_car(args))`。
   `s7_string` 底层是 `(T_Str(p))->object.string.svalue`（`s7_internal.h:1956`），
   `T_Str(P) = P`，对非字符串对象等同于把任意对象强转为字符串 cell 解引用。
   是否崩溃取决于垃圾指针落在哪：`wordexp`/`std::filesystem`/`std::string`
   这类会立刻解引用/strlen 的路径稳定崩溃；`tb_file_info` 等宽松路径只是静默返回错误值。

2. **长度/容量参数不校验**：`g_bytevector-base64-encode/decode` 的第二参数
   直接 `s7_integer(s7_cadr(args))`，与实际 bytevector 长度无关。

3. **Scheme 包装层不一致**：`(liii os)` 的 `rename`/`remove`/`listdir` 在包装层
   做了 `string?` 检查，但 `os-call`、`(liii sys)` 的 `which` 没有做，
   导致类型错误从 C 层的未定义行为漏出，而不是变成 `type-error`。

## 测试过但不会崩溃的方面（s7 核心防护完善）

- s7 自带 reader 对怪异 token（`#)` `#|` `#u8(300)` 等）全部干净报错
- 循环结构：`write`/`length`/`equal?` 都有环检测
- 深嵌套/深递归：s7 有栈保护；`(liii json)` 解析器有 10000 层深度限制；
  `(liii njson)` 底层 nlohmann::json 为迭代解析
- `vector-ref`/`string-ref` 越界、`make-vector -5`、`(string->number "1" 1)` 等均为干净报错
- `(liii base64)`、`(liii sort)`、`(liii string-cursor)`、`(liii subprocess)` 的
  C 层入口有 `s7_is_string`/`s7_is_integer` 检查

## 备注

`_readprobe/` 是扫描 reader token 时的临时目录残留，可删除。
