# (liii tui loader presets) 模块函数索引

`(liii tui loader presets)` 提供微调器（spinner）的内置动画帧样式集合与终态图标映射，支持 dots、line、pipe、star、bounce、pulse 与 ascii 预设。

使用时导入：

```scheme
(import (liii tui loader presets))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc spinner-presets
```

## 函数分类索引

### 动画帧预设与解析
- `spinner-presets` - 获取所有预设样式符号列表
- `spinner-frames-resolve` - 解析微调器动画帧列表

### 状态图标
- `spinner-status-icon` - 获取终态对应的状态指示图标
