# (scheme load) 模块函数分类索引

`(scheme load)` 提供运行时加载 Scheme 源文件的能力，适合插件加载、
脚本驱动和 REPL 场景中按需载入代码。

## 常见用法示例

```scheme
(import (scheme load) (scheme base))

;; 示例1：在交互环境中加载文件
(load "plugin/config.scm")

;; 示例2：在指定环境中加载文件
(load "plugin/config.scm" (environment '(scheme base)))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc scheme/load "函数名"
```

## 函数分类索引

### 一、文件加载

用于读取并求值文件内容的函数

- `load` - 读取文件并按序求值其中的每个 datum
