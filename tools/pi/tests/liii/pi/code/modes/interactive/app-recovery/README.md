# (liii pi code modes interactive app-recovery) 模块函数索引

`(liii pi code modes interactive app-recovery)` 负责 all-in-one 全链故障等待与
思考等级收敛的前台提示：recovery 事件原地更新同一张状态卡片，收敛提示追加
一次性卡片，两者都不写入会话 JSONL。

```scheme
(import (liii pi code modes interactive app-recovery))
```

## 函数分类索引

- `pi-app-handle-status-event!` - 把 recovery / thinking_clamp 事件翻译为状态卡片
- `pi-app-reset-recovery-card!` - 清除当前等待卡片引用
