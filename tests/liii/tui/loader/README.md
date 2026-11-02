# (liii tui loader) 模块函数索引

`(liii tui loader)` 统一导出异步操作进度与加载提示组件（spinner / loader），聚合自 `(liii tui loader spinner)`、`(liii tui loader state)` 与 `(liii tui loader presets)` 子模块。

支持多种预设动画帧（dots、line、pipe、star 等）、提示文本/前缀/后缀与刷新间隔动态更新、帧单步推进（tick）、显隐切换以及终态流转（成功/失败/警告/停止）。

使用时导入：

```scheme
(import (liii tui loader))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-spinner
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii tui loader spinner)` | 微调器组件构建与谓词，包装为标准 TUI component |
| `(liii tui loader state)` | 动画帧推进、文本修饰、显隐控制与状态流转管理 |
| `(liii tui loader presets)` | 内置动画帧预设集与状态图标映射 |

## 函数分类索引

### 微调器组件构建与谓词
- `make-spinner` - 创建微调器组件
- `spinner?` - 判断是否为微调器实例
- `make-loader` - 创建微调器组件（别名）
- `loader?` - 判断是否为加载器实例（别名）

### 动画帧与推进
- `spinner-frames` - 获取微调器动画帧列表
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

### 预设样式与图标
- `spinner-presets` - 获取所有预设样式符号列表
- `spinner-frames-resolve` - 解析微调器动画帧列表
- `spinner-status-icon` - 获取终态对应的状态指示图标
