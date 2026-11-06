# (liii random) 模块测试导览

`(liii random)` 是 `(srfi srfi-27)` 的重导出封装，提供随机整数、随机实数与随机源管理能力。

## 函数分类索引

### 一、基本随机数生成
- `random-integer` - 生成 [0, n-1] 范围内的随机整数
- `random-real` - 生成 (0, 1) 范围内的随机实数

### 二、随机源管理
- `make-random-source` - 创建新的随机源
- `random-source?` - 检查是否为随机源
- `default-random-source` - 默认随机源

### 三、随机源状态操作
- `random-source-state-ref` - 获取随机源状态
- `random-source-state-set!` - 设置随机源状态
- `random-source-randomize!` - 用当前时间随机化随机源
- `random-source-pseudo-randomize!` - 用索引伪随机化随机源

### 四、生成器创建
- `random-source-make-integers` - 从随机源创建整数生成器
- `random-source-make-reals` - 从随机源创建实数生成器

## 说明

各导出函数的按函数单元测试统一放在 `tests/srfi/srfi-27/` 目录；
本目录只保留面向 `(liii random)` 封装的集成用例 `reproducibility-test.scm`。
