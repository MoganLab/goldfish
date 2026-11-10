# (liii tui editor) 模块函数索引

`(liii tui editor)` 统一导出终端多行/单行文本行编辑器组件与底层子系统，聚合自 `(liii tui editor component)`、`(liii tui editor buffer)`、`(liii tui editor cursor)`、`(liii tui editor kill)`、`(liii tui editor handle)`、`(liii tui editor history)`、`(liii tui editor render)`、`(liii tui editor state)`、`(liii tui editor undo)`、`(liii tui editor view)`、`(liii tui editor viewport)` 与 `(liii tui editor paste)` 子模块。

使用时导入：

```scheme
(import (liii tui editor))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-editor
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii tui editor component)` | 通用行编辑器、单行输入框与多行编辑区组件 |
| `(liii tui editor buffer)` | 文本行缓冲区核心数据结构与编辑原语 |
| `(liii tui editor cursor)` | 光标步进、行首尾跳转与单词跳转 |
| `(liii tui editor kill)` | 整行剪切、词删除与字符互换操作 |
| `(liii tui editor handle)` | 键盘按键事件分发与处理 |
| `(liii tui editor history)` | 输入历史管理器与双向回溯 |
| `(liii tui editor render)` | 编辑器高级格式化渲染 |
| `(liii tui editor state)` | 编辑器内部数据记录结构 |
| `(liii tui editor undo)` | 状态快照与撤销回滚机制 |
| `(liii tui editor view)` | 逻辑光标折行视觉换算与软件光标绘制 |
| `(liii tui editor viewport)` | 滚动视口计算与光标可视同步 |
| `(liii tui editor paste)` | bracketed paste 识别、归一化与大段折叠展开 |

## 函数分类索引

### 编辑器组件构建与谓词
- `make-editor` - 创建通用行编辑器组件
- `editor?` - 判断是否为编辑器组件
- `make-text-input` - 创建单行文本输入框
- `text-input?` - 判断是否为单行输入框
- `make-text-area` - 创建多行文本编辑区
- `text-area?` - 判断是否为多行编辑区

### 内容与光标操作
- `editor-text` - 获取全文内容
- `editor-set-text!` - 设置全文内容
- `editor-expanded-text` - 获取展开粘贴标记后的完整内容
- `editor-lines` - 获取文本行列表
- `editor-cursor` - 获取光标坐标 (row col)
- `editor-set-cursor!` - 设置光标坐标
- `editor-display-col` - 获取光标物理显示列
- `editor-display-position` - 获取相对视口的光标显示位置

### 焦点与占位提示
- `editor-focused?` - 查询聚焦状态
- `editor-set-focused!` - 设置聚焦状态
- `editor-placeholder` - 获取占位提示
- `editor-set-placeholder!` - 设置占位提示

### 视口与外观
- `editor-max-lines` - 获取最大可视行数
- `editor-set-max-lines!` - 设置最大可视行数
- `editor-top-row` - 获取视口起始行号
- `editor-viewport` - 获取视口范围及上下隐藏行数
- `editor-border-color` - 获取边框颜色
- `editor-set-border-color!` - 设置边框颜色

### 历史记录与提交
- `editor-history` - 获取内部历史管理器
- `editor-history-list` - 获取历史条目列表
- `editor-set-on-submit!` - 设置提交回调
