# (liii pi code session-state) 模块函数分类索引

`(liii pi code session-state)` 提供恢复会话时投影状态的读取与合并能力。

使用时导入：

```scheme
(import (liii pi code session-state))
```

## 查看函数文档

```bash
gf doc pi-load-session-state
gf doc pi-resolve-session-state
gf doc pi-session-state-of
```

## 函数分类索引

### 投影状态
- `pi-load-session-state` - 从启动时恢复的会话文件读取投影状态
- `pi-resolve-session-state` - 将恢复的投影状态合并进 Agent 配置
- `pi-session-state-of` - 按会话文件路径读取投影状态
