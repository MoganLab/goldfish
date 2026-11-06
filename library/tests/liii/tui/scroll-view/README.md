# (liii tui scroll-view) 模块函数索引

`(liii tui scroll-view)` 提供在终端全屏表面承载可滚动内容的视口容器，支持内容自动跟随（follow 模式）、相对滚动、翻页、直达顶底以及缓存复用机制。

使用时导入：

```scheme
(import (liii tui scroll-view))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-scroll-view
```

## 函数分类索引

### 视口容器创建与谓词
- `make-scroll-view` - 创建滚动视口实例
- `scroll-view?` - 判断是否为滚动视口实例

### 内容管理与缓存
- `scroll-view-content` - 获取视口内容组件
- `scroll-view-set-content!` - 设置新的内容组件
- `scroll-view-invalidate!` - 清除内部内容行缓存

### 视口状态与位置
- `scroll-view-offset` - 获取当前行偏移量
- `scroll-view-set-offset!` - 显式设置行偏移量
- `scroll-view-follow?` - 判断是否处于内容跟随模式
- `scroll-view-set-follow!` - 开启或关闭跟随模式
- `scroll-view-total-lines` - 获取内容总行数
- `scroll-view-max-offset` - 获取最大有效偏移量
- `scroll-view-height` - 获取视口最近记录高度

### 滚动控制与渲染
- `scroll-view-render-lines` - 渲染视口可见行
- `scroll-view-scroll-by!` - 相对行数滚动
- `scroll-view-page-up!` - 向上翻页
- `scroll-view-page-down!` - 向下翻页
- `scroll-view-to-top!` - 滚动到最顶部
- `scroll-view-to-bottom!` - 滚动到最底部
