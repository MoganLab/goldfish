# (liii random) 模块测试导览

`(liii random)` 导出了 `(srfi srfi-27)` 的随机源接口，以及 s7 内置的 `random`、`random-state`、`random-state?`、`random-state->list` 过程。

## 函数分类索引

### 一、基本随机数生成
- `random` - 生成指定上界与类型的随机数（整数、实数、有理数或复数）
- `random-integer` - 生成 [0, n-1] 范围内的随机整数
- `random-real` - 生成 (0, 1) 范围内的随机实数

### 二、随机状态管理
- `random-state` - 创建随机数状态对象
- `random-state?` - 检查对象是否为随机状态
- `random-state->list` - 将随机状态转换为 `(seed carry)` 列表

### 三、随机源管理
- `make-random-source` - 创建新的随机源
- `random-source?` - 检查是否为随机源
- `default-random-source` - 默认随机源

### 四、随机源状态操作
- `random-source-state-ref` - 获取随机源状态
- `random-source-state-set!` - 设置随机源状态
- `random-source-randomize!` - 用当前时间随机化随机源
- `random-source-pseudo-randomize!` - 用索引伪随机化随机源

### 五、生成器创建
- `random-source-make-integers` - 从随机源创建整数生成器
- `random-source-make-reals` - 从随机源创建实数生成器

## 说明

从 `(srfi srfi-27)` 重导出函数的按函数单元测试统一放在 `library/tests/srfi/srfi-27/` 目录；
`(liii random)` 自身导出的 `random`、`random-state`、`random-state?`、`random-state->list` 单元测试及集成用例 `reproducibility-test.scm` 放在本目录。
