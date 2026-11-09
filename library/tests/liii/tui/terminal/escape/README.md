# (liii tui terminal escape) 模块函数索引

`(liii tui terminal escape)` 负责终端转义序列的字节级读取：判定帧完整性、原子化读取一帧、并把部分终端把 Alt+Enter 发成 `ESC CR LF` 时尾随的 LF 并回同一帧。

`(liii tui terminal poll)` 在此之上做按键分发与 bracketed paste。

使用时导入：

```scheme
(import (liii tui terminal escape))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "%read-escape-bytes"
```

## 函数分类索引

### 帧完整性判定
- `%escape-bytes-complete?` - 判断已累积字节是否构成完整转义帧

### 帧读取与 CRLF 合并
- `%read-escape-bytes` - 从控制台读取一个完整的转义序列字节帧
- `%escape-frame-merge` - 把 ESC CR 之后尾随的 LF 并入同一帧
