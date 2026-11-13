# (liii bigcomplex)

任意精度复数模块，支持以 `int64` / `<bigint>` / `<bigrat>` 为实部与虚部的 `<bigcomplex>` 表示。

## 接口列表

- `bigcomplex?` - 判断对象是否为 `<bigcomplex>`
- `make-bigcomplex` - 构造 `<bigcomplex>` 实例
- `bigcomplex-real` - 获取实部
- `bigcomplex-imag` - 获取虚部
- `bigcomplex->string` - 转换为复数字符串
