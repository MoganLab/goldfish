# (liii tui editor component) 模块函数索引

`(liii tui editor component)` 提供单行输入框（make-text-input）、多行编辑区（make-text-area）与通用行编辑器（make-editor）的高层组件抽象，支持光标导航、多行折行、边框修饰、占位提示、历史记录、粘贴展开与回车提交。

使用时导入：

```scheme
(import (liii tui editor component))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-editor
```

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
