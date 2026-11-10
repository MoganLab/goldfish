# (liii tui wheel-scroll) 模块函数索引

`(liii tui wheel-scroll)` 提供鼠标滚轮滚动加速器实现，支持根据滚轮物理事件的时间间隔自动加速计算滚动行数（auto 模式），也支持配置固定单次滚动行数。

使用时导入：

```scheme
(import (liii tui wheel-scroll))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-wheel-scroll-accelerator
```

## 函数分类索引

### 滚轮加速器创建与谓词
- `make-wheel-scroll-accelerator` - 创建滚轮加速器实例
- `wheel-scroll-accelerator?` - 判断对象是否为滚轮加速器实例

### 滚动计算与重置
- `wheel-scroll-next!` - 接收滚轮事件并计算本次滚动行数
- `wheel-scroll-reset!` - 重置滚轮加速器的内部手势状态

### 行数模式配置
- `wheel-scroll-lines` - 获取当前滚轮滚动行数模式
- `wheel-scroll-set-lines!` - 设置滚轮滚动行数模式（'auto 或正整数）
