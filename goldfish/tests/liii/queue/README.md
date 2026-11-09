# (liii queue) 模块导览

`(liii queue)` 提供基于列表的双端队列（list-queue），支持高效的两端操作，
是处理先进先出（FIFO）或双端访问场景的理想选择。

本库完整重导出 `(srfi srfi-117)`，不额外定义任何函数，
因此各函数的文档与单元测试位于 [../../srfi/srfi-117/README.md](../../srfi/srfi-117/README.md)。

## 常用用法

```scheme
(import (liii queue))

(define q (list-queue 1 2 3))
(list-queue-front q)         ; => 1
(list-queue-back q)          ; => 3
(list-queue-add-back! q 4)
(list-queue-remove-front! q) ; => 1
(list-queue-for-each display q)
```

## 查看函数文档

函数按 `(srfi srfi-117)` 归类，使用以下命令查看文档和用例：
```bash
gf doc srfi/srfi-117 "list-queue-front"
```
或直接按函数名检索：
```bash
gf doc "list-queue-add-back!"
```
