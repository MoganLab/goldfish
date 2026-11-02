# (liii tui box) 模块函数索引

`(liii tui box)` 统一导出高级布局与容器组件，聚合自 `(liii tui box box)`、`(liii tui box container)`、`(liii tui box hstack)`、`(liii tui box vstack)`、`(liii tui box mouse-region)` 与 `(liii tui box border)` 子模块。

使用时导入：

```scheme
(import (liii tui box))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-box
```

## 子模块导览

| 子模块 | 说明 |
|---|---|
| `(liii tui box box)` | 边框容器组件（内外边距与嵌入标题） |
| `(liii tui box container)` | 动态追加与清空的子组件容器 |
| `(liii tui box hstack)` | 水平分列排版组件 |
| `(liii tui box hstack width)` | 水平布局列宽计算算法 |
| `(liii tui box vstack)` | 垂直堆叠排版组件（支持间距与鼠标投影派发） |
| `(liii tui box mouse-region)` | 鼠标事件包装器（MouseRegion） |
| `(liii tui box border)` | 边框风格抽象与纯函数渲染 |

## 函数分类索引

### 边框容器组件
- `make-box` - 创建边框容器组件
- `box?` - 判断是否为边框容器组件

### 动态容器组件
- `make-container` - 创建动态容器实例
- `container?` - 判断是否为容器实例
- `container-add!` - 追加子组件或文本
- `container-clear!` - 清空所有子组件
- `container-children` - 获取当前子组件列表

### 排版布局组件
- `make-hstack` - 创建水平分列排版组件
- `hstack?` - 判断是否为水平分列组件
- `make-vstack` - 创建垂直堆叠排版组件
- `vstack?` - 判断是否为垂直堆叠组件

### 鼠标事件包装
- `make-mouse-region` - 创建鼠标区域包装组件
- `mouse-region?` - 判断是否为鼠标区域组件
- `mouse-region-child` - 获取包装的内部子组件

### 边框风格定义与渲染
- `make-border-style` - 创建自定义边框风格实例
- `border-style-resolve` - 解析边框风格配置
- `border-render-top` - 渲染顶边框（支持内嵌标题）
- `border-render-bottom` - 渲染底边框
- `border-render-line` - 内容行左右边框装饰
