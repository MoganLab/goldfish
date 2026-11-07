# (liii pi code modes config layout) 模块函数分类索引

`(liii pi code modes config layout)` 提供 ranger 风格三列配置视图的布局与渲染能力。

使用时导入：

```scheme
(import (liii pi code modes config layout))
```

## 查看函数文档

```bash
gf doc pi-config-column-widths
gf doc pi-config-cursor
gf doc pi-config-render
```

## 函数分类索引

### 布局与渲染
- `pi-config-column-widths` - 按终端宽度计算 ranger 风格三列的列宽
- `pi-config-cursor` - 将 provider 与模型下标规范化为光标位置
- `pi-config-render` - 渲染 ranger 风格的三列纯文本配置视图
