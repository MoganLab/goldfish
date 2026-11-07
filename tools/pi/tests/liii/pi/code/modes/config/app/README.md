# (liii pi code modes config app) 模块函数分类索引

`(liii pi code modes config app)` 提供 `gf pi config` 三列配置 TUI 的主循环与按键处理能力。

使用时导入：

```scheme
(import (liii pi code modes config app))
```

## 查看函数文档

```bash
gf doc pi-config-run
gf doc pi-config-key-handler
```

## 函数分类索引

### 配置 TUI
- `pi-config-run` - 运行配置 TUI 主循环并返回退出方式
- `pi-config-key-handler` - 处理配置 TUI 的按键，返回新的光标位置与动作
