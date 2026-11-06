# (liii tui box vstack) 模块函数索引

`(liii tui box vstack)` 提供垂直堆叠布局组件，将多个子组件纵向依次拼接排版，支持设置子组件间的纵向空行间距、按键事件链式分发与基于行高区间投影的局部鼠标事件命中派发。

使用时导入：

```scheme
(import (liii tui box vstack))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-vstack
```

## 函数分类索引

### 垂直布局组件与谓词
- `make-vstack` - 创建垂直堆叠排版组件
- `vstack?` - 判断是否为垂直堆叠组件

### 内部辅助与渲染原语
- `%validate-child` - 校验并规范化单个子组件
- `%vstack-render` - 纯函数垂直渲染
- `%vstack-render-layout` - 渲染并量取各子组件行高
- `%vstack-dispatch-mouse` - 依据行高区间派发鼠标事件
- `%vstack-handle-input` - 链式分发按键输入
- `%vstack-handle-mouse` - 从布局缓存派发鼠标事件
- `%vstack-invalidate` - 递归使所有子组件脏区失效
