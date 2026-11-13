# (liii bigrat)

任意精度有理数模块，基于 `(liii bigint)` 提供规范化分数表示 `<bigrat>` 与基础运算。

## 接口列表

- `bigrat?` - 判断对象是否为 `<bigrat>`
- `make-bigrat` - 构造并约分规范化有理数
- `bigrat-num` - 获取分子
- `bigrat-den` - 获取分母
- `bigrat->string` - 转换为分数字符串
- `bigrat-positive?` - 正数判断
- `bigrat-negative?` - 负数判断
- `bigrat=?` - 相等判断
- `bigrat<?` - 小于判断
