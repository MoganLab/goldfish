# (liii bignum)

`(liii bignum)` 是 Goldfish Scheme 的运行时任意精度数值塔适配库，**使用及测试时必须开启 `+bignum` 模式**（例如 `bin/gf --mode=r7rs+bignum` 或 `bin/gf --mode=liii+bignum`），以启用 C 层 Reader/Writer 的大数、大分数、精确复数与 `#e`/`#i` 字面量解析。

## 概述

该库基于 `(liii bigint)`、`(liii bigrat)`、`(liii bigcomplex)` 与 `(liii decimal)` 覆写了 `(scheme base)`、`(scheme inexact)`、`(scheme complex)` 中的数值谓词、比较、算术与转换过程，在 `int64` 溢出时自动提升为 `<bigint>`、`<bigrat>`、`<bigcomplex>`，并在结果可表示时自动降级为原生精确数值。

## 文档查询

```bash
bin/gf doc liii/bignum
bin/gf doc liii/bignum "+"
```
