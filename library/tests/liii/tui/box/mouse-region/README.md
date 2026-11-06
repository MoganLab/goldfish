# (liii tui box mouse-region) 模块函数索引

`(liii tui box mouse-region)` 提供鼠标事件包装组件（MouseRegion），可在不改变子组件渲染与键盘行为的前提下为其注入鼠标事件监听回调。

使用时导入：

```scheme
(import (liii tui box mouse-region))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-mouse-region
```

## 函数分类索引

### 鼠标区域组件与谓词
- `make-mouse-region` - 创建鼠标区域包装组件
- `mouse-region?` - 判断是否为鼠标区域组件
- `mouse-region-child` - 获取包装的内部子组件
