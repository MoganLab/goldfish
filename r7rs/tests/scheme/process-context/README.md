# (scheme process-context) 模块函数分类索引

`(scheme process-context)` 提供访问进程环境的能力，
包括读取单个环境变量、枚举全部环境变量以及获取命令行参数。
它适合脚本编程、配置读取和命令行工具等场景。

## 常见用法示例

```scheme
(import (scheme process-context))

;; 示例1：读取单个环境变量
(get-environment-variable "PATH")   ; => 字符串，不存在时为 #f

;; 示例2：枚举全部环境变量
(assoc "PATH" (get-environment-variables))   ; => ("PATH" . "...")

;; 示例3：获取命令行参数
(command-line)                      ; => ("gf" "test" ...)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc scheme/process-context "函数名"
```

## 函数分类索引

### 一、环境变量

- `get-environment-variable` - 读取指定环境变量的值
- `get-environment-variables` - 返回全部环境变量的键值对列表

### 二、命令行

- `command-line` - 返回当前程序的命令行参数列表

### 三、进程退出

- `exit` - 运行 dynamic-wind after 后退出进程
- `emergency-exit` - 跳过所有清理立即退出进程
