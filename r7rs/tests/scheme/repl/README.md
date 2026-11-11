# (scheme repl) 模块函数分类索引

`(scheme repl)` 提供交互式环境获取能力，适合 REPL、增量脚本执行以及动态交互求值场景。

## 常见用法示例

```scheme
(import (scheme repl) (scheme eval) (scheme base))

;; 示例1：获取交互环境并在其中求值表达式
(define env (interaction-environment))
(eval '(+ 1 2) env)             ; => 3

;; 示例2：在交互环境中动态定义新绑定
(eval '(define answer 42) env)
answer                          ; => 42
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc scheme/repl "函数名"
```

## 函数分类索引

### 一、交互环境

用于获取交互式求值环境说明符的函数

- `interaction-environment` - 获取交互式求值环境对象（顶层环境）
