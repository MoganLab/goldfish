# (liii subprocess registry) 模块导览

`(liii subprocess registry)` 提供子进程命令注册表与安全访问控制机制，负责符号命令的别名映射、查询以及命令黑白名单过滤。

使用时导入：

```scheme
(import (liii subprocess registry))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/subprocess/registry "函数名"
```

## 函数分类索引

### 一、符号命令注册与查询
- `run-set!` - 为符号命令注册可执行路径或过程
- `run-get` - 查询符号命令的注册值

### 二、命令黑白名单访问控制
- `run-allow!` - 设置符号命令执行白名单
- `run-ban!` - 禁止执行指定的符号或字符串命令
- `run-unban!` - 解除对指定命令的禁止

### 三、内部命令解析与校验
- `%check-string-command` - 校验字符串命令是否命中黑名单
- `%resolve-symbol-command` - 解析符号命令并根据黑白名单校验权限
