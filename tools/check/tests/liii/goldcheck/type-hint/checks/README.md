# (liii goldcheck type-hint checks) 模块函数分类索引

type-hint 的 A 类结构校验（库名、标准查询函数、导入限制、签名语法合法性）与 B2 原库交叉参数数量一致性检查。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、Hint 库自身结构校验（A 类）
- `check-a1` - 校验 hint 库名是否与文件路径严格对应
- `check-a2` - 校验是否导出了约定命名的标准查询函数
- `check-a3` - 校验 hint 库是否违规导入了其它 hint 库
- `check-var` - 校验 hint 变量定义及其签名语法的合法性（A4/A6）
- `check-a5` - 校验查询函数 case 分支与 hint 变量定义是否一一对应

### 二、原库交叉参数数量一致性校验（B 类）
- `check-b` - 逐函数校验 hint 签名参数数量是否与原库函数定义的 arity 一致（B2）
