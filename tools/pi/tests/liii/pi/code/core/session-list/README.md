# (liii pi code core session-list) 模块函数分类索引

`(liii pi code core session-list)` 提供会话文件信息的读取、汇总与排序能力。

使用时导入：

```scheme
(import (liii pi code core session-list))
```

## 查看函数文档

```bash
gf doc pi-read-session-info
gf doc pi-list-session-infos
gf doc make-pi-session-info
```

## 函数分类索引

### 会话信息读取
- `pi-read-session-info` - 读取会话文件并解析为会话信息记录
- `pi-list-session-infos` - 列出会话目录中的全部会话信息记录

### 会话信息记录
- `make-pi-session-info` - 创建会话信息记录
- `pi-session-info?` - 判断对象是否为会话信息记录
- `pi-session-info-path` - 读取会话信息记录中的会话文件路径
- `pi-session-info-id` - 读取会话信息记录中的会话 ID
- `pi-session-info-cwd` - 读取会话信息记录中的工作目录
- `pi-session-info-name` - 读取会话信息记录中的会话名称
- `pi-session-info-created` - 读取会话信息记录中的创建时间
- `pi-session-info-modified` - 读取会话信息记录中的修改时间
- `pi-session-info-message-count` - 读取会话信息记录中的消息数量
- `pi-session-info-first-message` - 读取会话信息记录中的首条消息
- `pi-session-info-all-messages-text` - 读取会话信息记录中的全部消息文本

### 排序
- `pi-session-info-newer?` - 判断一个会话信息记录是否比另一个更新
