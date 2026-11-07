# (liii pi code core session-name) 模块函数分类索引

`(liii pi code core session-name)` 提供会话名称的解析、记录与初始化能力。

使用时导入：

```scheme
(import (liii pi code core session-name))
```

## 查看函数文档

```bash
gf doc get-session-name
gf doc resolve-initial-session-name
gf doc append-session-info!
```

## 函数分类索引

### 会话名称
- `get-session-name` - 读取会话文件中记录的会话名称
- `resolve-initial-session-name` - 解析新会话的初始名称
- `append-session-info!` - 向会话文件追加一条会话名称记录
- `setup-session-file-with-name` - 创建带名称的会话文件并写入头部与名称条目
