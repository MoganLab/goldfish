# (liii bignum)

`(liii bignum)` 是 Goldfish Scheme 的运行时任意精度整数数值塔适配库，配合 `--mode=liii+bignum`、`--mode=r7rs+bignum` 等模式使用。

## 概述

该库基于 `(liii bigint)` 覆写了 `(scheme base)` 中的数值谓词、比较、算术与转换过程，在 `int64` 溢出时自动提升为 `<bigint>`，并在结果落回 `[INT64_MIN, INT64_MAX]` 时自动降级为原生精确整数。
