# (liii format color) 模块测试导览

`(liii format color)` 在 `(srfi srfi-166 color)` 基础上重导出颜色组合子，
并增强 `as-true-color` / `on-true-color`：颜色既可以用 RGB 列表或 hex 字符串
作为单个值传递，也兼容 srfi-166 的散参形式。

| 函数 | 测试文件 |
| --- | --- |
| `as-true-color` | `as-true-color-test.scm` |
| `on-true-color` | `on-true-color-test.scm` |
| 其余颜色组合子（重导出） | `tests/srfi/srfi-166/color/` |

## 查看函数文档

```bash
gf doc liii/format/color "as-true-color"
```
