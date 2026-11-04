# (liii subprocess registry) 模块函数分类索引

`(liii subprocess registry)` 提供命令符号注册映射与执行权限控制，包括命令符号别名解析、黑白名单访问控制以及命令校验。

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
- `run-set!` - 为符号命令注册解析方式
- `run-get` - 查询符号命令的注册值

### 二、命令黑白名单访问控制
- `run-allow!` - 设置符号命令白名单
- `run-ban!` - 显式禁止某个符号命令或字符串命令
- `run-unban!` - 取消对某个符号命令的禁止

### 三、内部命令解析与校验
- `%check-string-command` - 校验字符串命令是否命中了黑名单
- `%resolve-symbol-command` - 解析符号命令并根据黑白名单进行安全校验
