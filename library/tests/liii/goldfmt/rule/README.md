# (liii goldfmt rule) 模块函数索引

`(liii goldfmt rule)` 提供 goldfmt 的排版规则查询能力：从 `node-rules.json` 中按 tag 名称读取内联（inline）策略、首行保留数量、缩进方式等规则，并提供规则文件路径的查找接口。

使用时导入：

```scheme
(import (liii goldfmt rule))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/rule "函数名"
```

## 函数分类索引

### 规则文件查找
- `find-node-rules-paths` - 在 load-path 中查找所有可用的 node-rules.json 文件路径
- `find-node-rules-path` - 查找当前生效的 node-rules.json 文件路径

### 内联策略
- `max-inline-length` - formatter 默认允许 inline 的最大字符串长度
- `must-inline?` - 判断某个 tag 是否必须单行输出
- `never-inline?` - 判断某个 tag 是否禁止单行输出
- `never-inline-when-first-child-env?` - 判断某个 tag 在首个 child 是 env 时是否禁止单行输出
- `allow-first-line-child-env?` - 判断某个 tag 是否允许首行保留 env 类型的 child

### 排版限制
- `first-line-limit` - 读取某个 tag 在第一行最多保留的 child 数量
- `second-child-tree-depth-limit` - 读取某个 tag 第二棵子树允许的最大深度
- `rest-indent` - 读取某个 tag 其余 child 的缩进方式

### 路径工具
- `path-has-common-ancestor?` - 判断两个路径是否具有公共祖先目录
