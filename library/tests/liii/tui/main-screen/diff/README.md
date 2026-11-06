# (liii tui main-screen diff) 模块函数索引

`(liii tui main-screen diff)` 提供主屏幕的行级差分算法，比较前后两帧文本行并生成 keep/update/append/clear 操作序列，同时支持脏状态下的全量重绘差分。

使用时导入：

```scheme
(import (liii tui main-screen diff))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc screen-diff
```

## 函数分类索引

### 差分计算
- `screen-diff` - 比较两帧文本行并生成差分操作
- `screen-dirty-diff` - 脏状态下的全量更新差分

### 差分操作访问器
- `diff-op-type` - 获取差分操作类型
- `diff-op-index` - 获取差分操作作用的行索引
- `diff-op-text` - 获取差分操作携带的文本内容

### 差分结果判定
- `screen-diff-has-changes?` - 判断差分序列是否存在内容变化
