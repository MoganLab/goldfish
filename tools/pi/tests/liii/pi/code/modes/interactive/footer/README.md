# (liii pi code modes interactive footer) 模块函数索引

`(liii pi code modes interactive footer)` 提供交互模式底部状态栏组件（Footer）。

使用时导入：

```scheme
(import (liii pi code modes interactive footer))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc <函数名>
```

## 函数分类索引

### 组件构建与类型判定
- `make-pi-footer` - 创建底部状态栏组件
- `pi-footer?` - 判断是否为底部状态栏组件

### 状态读取与设置
- `format-cwd-for-footer` - 格式化工作目录显示
- `pi-footer-branch` - 获取当前显示分支名
- `pi-footer-set-branch!` - 设置分支名
- `pi-footer-set-session-name!` - 设置会话名称
- `pi-footer-set-model!` - 设置模型与供应商
- `pi-footer-set-thinking-level!` - 设置思考等级
- `pi-footer-message` - 获取临时提示消息
- `pi-footer-set-message!` - 设置临时提示消息
- `pi-footer-stats` - 获取当前设置的统计字符串
- `pi-footer-set-stats!` - 设置统计字符串
- `pi-footer-set-token-stats!` - 根据累计 Token、最新命中率与上下文占用设置统计行（含 CHxx.x% 与 x.x%/1.0M）
- `pi-footer-set-context-unknown!` - 上下文重建（压缩）后将占比置为未知（显示 "?"）
- `pi-footer-context-window` - 获取当前上下文窗口容量（Token 数）
