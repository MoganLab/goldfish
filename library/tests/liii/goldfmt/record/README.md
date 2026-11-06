# (liii goldfmt record) 模块函数索引

`(liii goldfmt record)` 提供 goldfmt 格式化过程中的记录类型定义：env（带括号的结构）、atom（无括号的原子）、原始字符串字面量、字符字面量与点尾部，并提供 env 树结构合法性检查。

使用时导入：

```scheme
(import (liii goldfmt record))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/record "函数名"
```

## 函数分类索引

### env 记录
- `make-env` - 创建环境记录对象
- `env?` - 判断一个值是否为 env 记录
- `env-tag-name` - 获取 env 节点的标记名称
- `env-depth` - 获取 env 节点的深度
- `env-indent` - 获取 env 节点的缩进量
- `env-children` - 获取 env 节点的子节点列表
- `env-left-line` - 获取 env 节点左侧标记所在的行号
- `env-right-line` - 获取 env 节点右侧标记所在的行号
- `env-value` - 获取 env 节点保存的值

### atom 记录
- `make-atom` - 创建 atom 记录对象
- `atom?` - 判断一个值是否为 atom 记录
- `atom-depth` - 获取 atom 节点的深度
- `atom-indent` - 获取 atom 节点的缩进量
- `atom-left-line` - 获取 atom 节点起始文本所在的行号
- `atom-right-line` - 获取 atom 节点结束文本所在的行号
- `atom-value` - 获取 atom 节点保存的值

### 字面量与点尾部记录
- `make-raw-string-literal` - 创建原始字符串字面量记录
- `raw-string-literal?` - 判断一个值是否为原始字符串字面量记录
- `raw-string-literal-source` - 获取原始字符串字面量的源码文本
- `raw-string-literal-value` - 获取原始字符串字面量解析后的字符串值
- `make-char-literal` - 创建字符字面量记录
- `char-literal?` - 判断一个值是否为字符字面量记录
- `char-literal-source` - 获取字符字面量的源码文本
- `char-literal-value` - 获取字符字面量解析后的字符值
- `make-dotted-tail` - 创建带点尾部的记录
- `dotted-tail?` - 判断一个值是否为点尾部记录
- `dotted-tail-form` - 获取点尾部记录所指向的 form

### 结构合法性检查
- `good-env?` - 检查一棵 env 树是否满足基本结构约束
- `assert-env` - 断言一棵 env 树满足基本结构约束
