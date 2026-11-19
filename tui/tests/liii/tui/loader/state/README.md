# (liii tui loader state) 模块函数索引

`(liii tui loader state)` 提供微调器（spinner）的内部状态管理，覆盖动画帧推进、文本/前缀/后缀/刷新间隔动态更新、显隐切换、终态流转（成功/失败/警告/停止）以及状态变更通知机制。

使用时导入：

```scheme
(import (liii tui loader state))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc spinner-status
```

## 函数分类索引

### 内部解包与通知
- `%unwrap-spinner` - 解包微调器获取底层私有记录
- `%notify-spinner-change!` - 触发状态变更回调

### 动画帧与索引
- `spinner-frames` - 获取动画帧列表
- `spinner-frame-index` - 获取当前帧索引
- `spinner-current-frame` - 获取当前帧字符或终态图标
- `spinner-tick!` - 推进一帧动画
- `spinner-reset!` - 复位帧索引并恢复旋转状态

### 文本与修饰
- `spinner-text` - 获取提示文本
- `spinner-set-text!` - 设置提示文本
- `spinner-prefix` - 获取前缀文本
- `spinner-set-prefix!` - 设置前缀文本
- `spinner-suffix` - 获取后缀文本
- `spinner-set-suffix!` - 设置后缀文本
- `spinner-interval` - 获取刷新间隔
- `spinner-set-interval!` - 设置刷新间隔

### 显隐控制
- `spinner-visible?` - 查询是否可见
- `spinner-set-visible!` - 设置可见性
- `spinner-show!` - 显示微调器
- `spinner-hide!` - 隐藏微调器

### 状态流转
- `spinner-status` - 获取当前状态
- `spinner-set-status!` - 显式设置状态
- `spinner-success!` - 置为成功状态
- `spinner-fail!` - 置为失败状态
- `spinner-error!` - 置为错误状态
- `spinner-warn!` - 置为警告状态
- `spinner-stop!` - 置为停止状态
