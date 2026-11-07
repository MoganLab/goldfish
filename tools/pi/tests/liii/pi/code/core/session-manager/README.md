# (liii pi code core session-manager) 模块函数分类索引

`(liii pi code core session-manager)` 提供会话文件的创建、目录解析、条目读写与消息持久化能力。

使用时导入：

```scheme
(import (liii pi code core session-manager))
```

## 子库导览

- `append/` - 会话条目追加与消息 ID 查找
- `messages/` - 会话条目到上下文消息的映射
- `normalize/` - 会话消息规范化

## 查看函数文档

```bash
gf doc get-session-dir
gf doc make-session-file-path
gf doc setup-session-file
```

## 函数分类索引

### 会话目录与路径
- `encode-cwd-to-session-dirname` - 将工作目录路径编码为会话目录名
- `get-session-dir` - 获取当前工作目录对应的会话目录路径
- `get-sessions-root` - 获取会话根目录路径
- `resolve-session-dir` - 解析当前会话使用的会话目录
- `make-session-file-path` - 生成会话文件的完整路径
- `find-latest-session-file` - 查找会话目录中最近修改的会话文件
- `resolve-resume-session-file` - 解析需要恢复的会话文件路径

### 会话文件读写
- `read-session-history` - 读取会话文件中的历史记录
- `save-session-header!` - 写入会话文件头条目
- `save-session-message!` - 写入一条会话消息条目
- `setup-session-file` - 创建会话文件并写入头部条目
- `setup-initial-messages` - 将会话初始消息写入会话文件
- `persist-new-messages!` - 将新产生的消息持久化到会话文件
- `append-bash-execution!` - 向会话文件追加一条 bash 执行记录
