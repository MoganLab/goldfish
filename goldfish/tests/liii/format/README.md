# (liii format) 模块测试导览

`(liii format)` 是 `(srfi srfi-166)` 的封装与增强层，按子模块拆分：

| 子模块 | 说明 | 测试目录 |
| --- | --- | --- |
| `(liii format base)` | 基础格式化接口（`show`、`displayed`、`escaped` 等） | `tests/srfi/srfi-166/base/` |
| `(liii format color)` | 颜色格式化组合子（前景/背景/真彩色） | `tests/liii/format/color/`、`tests/srfi/srfi-166/color/` |
| `(liii format columnar)` | 表格与分栏格式化 | `tests/srfi/srfi-166/columnar/` |
| `(liii format pretty)` | 漂亮打印 | `tests/srfi/srfi-166/pretty/` |
| `(liii format unicode)` | 终端宽度与 Unicode 感知格式化 | `tests/srfi/srfi-166/unicode/` |

## 查看函数文档

```bash
gf doc liii/format/base
gf doc liii/format/color
```
