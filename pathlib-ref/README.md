# pathlib 真值参考

为 goldfish `(liii path)` 库的对齐工作提供 Python `pathlib` 的权威行为基准,
并附带 goldfish 审计探针与一键对照工具。

## 文件

- `posix_ref.py` — `PurePosixPath` 真值(对应 POSIX 平台 / macOS / Linux)
- `windows_ref.py` — `PureWindowsPath` 真值(对应 Windows 平台)
- `posix_audit.scm` — goldfish POSIX 实际行为审计(label 与 posix_ref.py 一一对应)
- `windows_audit.scm` — goldfish Windows 实际行为审计(label 与 windows_ref.py 一一对应)
- `normalize.py` — 一键对照工具,归一化两边输出后只打印差异

## 背景

pathlib 的行为是平台相关的:`Path("C:\\a")` 在 POSIX 上是 `PurePosixPath`(整体当一段),
在 Windows 上是 `PureWindowsPath`(drive=`C:`、有解析)。goldfish 已按 `os-windows?` 分流
`path-type` 复刻此差异。

因此审计分两套:
- **POSIX** — 在 macOS/Linux 上跑,对照 `PurePosixPath`。
- **Windows** — 须在 Windows 平台跑(`(os-windows?)=>#t` 时字符串才解析为 windows record),
  对照 `PureWindowsPath`。macOS 上跑 `windows_audit.scm` 字符串会走 posix 解析,结果无意义。

## 一键对照(推荐)

```bash
# POSIX(任意平台均可)
python3 pathlib-ref/normalize.py posix

# Windows(须在 Windows 平台)
python3 pathlib-ref/normalize.py windows
```

输出 `=== posix: 共 N 项,差异 0 ===` 即完全对齐;否则列出每个 `[差异]` / `[仅 python]` /
`[仅 scheme]`,据此修正实现或测试。

## 手动流程

1. 跑参考脚本拿到 pathlib 真值:
   ```bash
   python3 pathlib-ref/posix_ref.py
   python3 pathlib-ref/windows_ref.py
   ```

2. 跑 goldfish 审计探针:
   ```bash
   bin/gf pathlib-ref/posix_audit.scm
   bin/gf pathlib-ref/windows_audit.scm   # Windows 平台
   ```

3. 逐行 diff:
   ```bash
   diff <(python3 pathlib-ref/posix_ref.py) <(bin/gf pathlib-ref/posix_audit.scm)
   ```

## 输出格式与归一化

每行 `label => value`。Python 用 `repr`(`'...'`/`[...]`/`True`/`False`),
Scheme 用 `write`(`"..."`/`(...)`/`#t`/`#f`)。`normalize.py` 会把两边 value 归一化
(去引号、list 括号统一为 `()`、布尔统一为 `#t`/`#f`)后按 label 比对,只打印真差异。

## 验证范围

覆盖 path 库全部 API 的边界用例:`path->string`、`name`、`stem`、`suffix`、`suffixes`、
`parent`、`parents`、`parts`、`join`、`match`、`absolute?`、`relative-to`、
`with-name/stem/suffix`、`as-posix`,以及 Windows 的 `drive`/`root`、UNC、drive-relative 等。

边界包括:`.`、`..`、空串、尾斜杠、连续点(`a..b`/`a...b`)、末尾点(`foo.`)、
隐藏文件(`.bashrc`)、多后缀(`a.tar.gz`)、绝对模式 match、空串 join 等。
