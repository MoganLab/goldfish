# (liii goldcheck type-hint narrow) 模块函数分类索引

基于条件守卫（if、and、cond）的局部变量类型收窄推导引擎。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、类型与环境收窄
- `narrow-type` - 按真值假设或具体谓词对类型表达式进行收窄（剔除 #f）
- `narrow-locals` - 对局部变量环境中的指定变量收窄其类型
- `test-assumptions` - 从测试条件表达式提取收窄假设列表
- `apply-assumptions` - 将收窄假设列表批量应用到局部变量环境

### 二、控制流 AST 遍历收窄
- `walk-if` - 遍历 if 表达式，在 then 分支应用测试条件的收窄
- `walk-and` - 遍历 and 表达式，后置操作数依据前置操作数逐级收窄
- `walk-cond` - 遍历 cond 表达式，各分支依其子句测试条件收窄
