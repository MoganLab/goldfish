# (liii tui select-list) 模块函数索引

`(liii tui select-list)` 提供可交互的终端单选列表组件，支持方向键导航、循环滚动、分页与滚动指示器、详细描述渲染以及选中/取消事件监听。

使用时导入：

```scheme
(import (liii tui select-list))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-select-list
```

## 函数分类索引

### 选择列表组件构建与谓词
- `make-select-list` - 创建选择列表组件
- `select-list?` - 判断是否为选择列表组件

### 列表项与选中状态访问
- `select-list-items` - 获取当前列表条目
- `select-list-selected-index` - 获取当前选中条目的索引
- `select-list-selected-item` - 获取当前选中的条目
- `select-list-set-selected-index!` - 设置当前选中的条目索引
