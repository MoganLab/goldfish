# (liii tui terminal virtual state) 模块函数索引

`(liii tui terminal virtual state)` 提供虚拟终端内部状态的数据结构与字符级绘制原语，包括屏幕行向量构造、光标推进、字符写入、行复制滚动以及 ANSI CSI 序列跳过。

使用时导入：

```scheme
(import (liii tui terminal virtual state))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-vt-state
```

## 函数分类索引

### 屏幕行构造与尺寸校验
- `make-char-row` - 构造一行空白字符向量
- `make-screen-lines` - 构造屏幕行向量
- `check-dimension` - 校验尺寸参数为正整数

### 字符绘制原语
- `put-char!` - 在光标位置写入字符并右移
- `advance-cursor-line!` - 光标下移一行并在到底时上滚
- `scroll-up-one-line!` - 屏幕内容整体上移一行
- `copy-row-chars!` - 复制一行字符到目标行

### ANSI 序列处理
- `%skip-ansi-csi` - 跳过 ANSI CSI 转义序列
- `%vt-handle-char!` - 处理单个字符（回车/换行/普通字符）
