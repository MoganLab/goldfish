# (liii subprocess registry) 模块函数分类索引

`(liii subprocess registry)` 模块提供符号命令的注册、查询与黑白名单安全控制机制，供子进程执行体系管理可信命令映射。

## 查看函数文档

使用以下命令查看子模块文档：
```bash
gf doc liii/subprocess/registry
```
或者查看具体函数的文档和用例：
```bash
gf doc liii/subprocess/registry "run-set!"
```

## 函数分类索引

### 一、注册与查询
- `run-set!` - 为符号命令注册可执行路径或自定义过程
- `run-get` - 获取已注册符号的解析值

### 二、权限与黑白名单控制
- `run-allow!` - 设置符号命令执行白名单
- `run-ban!` - 显式禁止符号或前缀字符串命令
- `run-unban!` - 解除对符号命令的禁止

### 三、内部命令解析与校验
- `%check-symbol-command` - 校验符号命令是否被禁止或在白名单中
- `%check-string-command` - 校验字符串命令是否匹配黑名单前缀
- `%resolve-symbol-command` - 解析符号命令为可执行项及参数列表
