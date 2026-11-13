# `bin/gf --mode r7rs+bignum` 与 Chez Scheme (`scheme`) Bignum 行为对比差异

对比环境：
- Goldfish Scheme：`bin/gf --mode r7rs+bignum eval ...`（`Goldfish Scheme 18.11.57`）
- Chez Scheme：`scheme -q`（`10.4.1`）

---

## 1. 精确复数塔缺失（bignum 尚未支持 `<bigcomplex>`）

Goldfish 的 `bignum` 模式已支持任意精度整数（`<bigint>`）与任意精度有理数（`<bigrat>`），但尚未扩展到任意精度复数；当与复数混合运算或读取超大实部/虚部复数字面量时，会退化为 `inexact` 浮点复数（或由 s7 按 `int64` 回绕解析）。

| 表达式 | `gf` 输出 | `scheme` 输出 | 说明 |
| --- | --- | --- | --- |
| `(+ 100000000000000000000 1+2i)` | `1e+20+2.0i` | `100000000000000000001+2i` | `bigint` 与复数运算退化为浮点复数 |
| `(= 100000000000000000000 100000000000000000000+0i)` | `#f` | `#t` | 复数字面量未接入 bignum reader，实部按 `int64` 回绕 |

---

## 2. 输出格式与小细节差异

| 类别 | 表达式 | `gf` 输出 | `scheme` 输出 | 说明 |
| --- | --- | --- | --- | --- |
| 16 进制大小写 | `(number->string 123456789012345678901234567890 16)` | `"18ee90ff6c373e0ee4e3f0ad2"` | `"18EE90FF6C373E0EE4E3F0AD2"` | `gf` 输出小写，`scheme` 输出大写 |
| 科学计数法显示 | `(inexact 100000000000000000000)` | `1e+20` | `1e20` | `gf` 带 `+` 号，数值等价 |
| `exact-integer?` | `(exact-integer? 11111111111111111111111111111111111)` | `#t` | 报错 `variable exact-integer? is not bound` | `exact-integer?` 是 R7RS 过程，Chez Scheme 默认按 R6RS 顶层环境未导出 |
