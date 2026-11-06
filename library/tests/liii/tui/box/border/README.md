# (liii tui box border) 模块函数索引

`(liii tui box border)` 提供 TUI 边框风格抽象与纯函数渲染能力，支持 single、double、rounded、ascii 与 none 预设，并支持自定义边框字符、内嵌标题顶边框与内容左右边线包装。

使用时导入：

```scheme
(import (liii tui box border))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-border-style
```

## 函数分类索引

### 边框风格创建与解析
- `make-border-style` - 创建自定义边框风格实例
- `border-style-resolve` - 解析边框风格配置（符号或实例）

### 边框渲染原语
- `border-render-top` - 渲染顶边框（支持内嵌标题）
- `border-render-bottom` - 渲染底边框
- `border-render-line` - 为单行内容加上左右竖线与补齐空格
