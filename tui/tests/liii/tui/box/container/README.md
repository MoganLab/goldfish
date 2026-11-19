# (liii tui box container) 模块函数索引

`(liii tui box container)` 提供动态子组件容器，支持通过追加（add）和清空（clear）管理子组件列表，自动将字符串包装为文本组件，并支持鼠标事件分发与脏区传递。

使用时导入：

```scheme
(import (liii tui box container))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-container
```

## 函数分类索引

### 容器构建与谓词
- `make-container` - 创建动态容器实例
- `container?` - 判断是否为容器实例

### 子组件管理
- `container-add!` - 追加子组件或文本
- `container-clear!` - 清空所有子组件
- `container-children` - 获取当前子组件列表
