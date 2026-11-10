# (liii pi code core context-usage) 模块函数索引

`(liii pi code core context-usage)` 负责上下文占用估算：由单轮 usage 换算上下文 Token 数、
占上下文窗口的百分比，并格式化为页脚左下角的 `x.x%/1.0M (auto)` 描述
（对齐上游 pi 的 `getContextUsage` 与 Footer 显示）。

使用时导入：

```scheme
(import (liii pi code core context-usage))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc <函数名>
```

## 函数分类索引

### Token 换算与占比
- `pi-calculate-context-tokens` - 由单轮 usage 换算上下文 Token 数（totalTokens 优先）
- `pi-context-percent` - 计算上下文 Token 数占窗口的百分比

### 上下文占用 record
- `make-pi-context-usage` - 创建上下文占用 record（百分比、窗口容量、是否已知）
- `pi-context-usage?` - 判断是否为上下文占用 record
- `pi-context-usage-percent` - 获取上下文占比
- `pi-context-usage-window` - 获取上下文窗口容量
- `pi-context-usage-known?` - 判断占比是否已知（压缩后尚无新响应时为 #f）

### 格式化与恢复
- `pi-format-context-usage` - 格式化为 `x.x%/1.0M (auto)`（未知时显示 `?`）
- `pi-session-context-usage` - 从会话文件恢复上下文占用（压缩后无新响应时占比未知）
