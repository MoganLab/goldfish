# (liii enum) 模块导览

`(liii enum)` 提供枚举类型、枚举值与枚举集合（enum-set）的完整抽象，
适合状态机、有限选项、权限集合和可排序的命名常量。

本库完整重导出 `(srfi srfi-209)`，不额外定义任何函数，
因此各函数的文档与单元测试位于 [../../srfi/srfi-209/README.md](../../srfi/srfi-209/README.md)。

## 常用用法

```scheme
(import (liii enum))

;; 示例1：定义一个枚举类型并按名字取值
(define Color (make-enum-type '(red green blue)))
(enum-name (enum-name->enum Color 'green))

;; 示例2：读取枚举类型的大小
(enum-type-size Color)

;; 示例3：把枚举值放入 enum-set 再做成员判断
(enum-set-contains? (enum-set Color (enum-name->enum Color 'red))
  (enum-name->enum Color 'red)
)
```

## 查看函数文档

函数按 `(srfi srfi-209)` 归类，使用以下命令查看文档和用例：
```bash
gf doc srfi/srfi-209 "make-enum-type"
```
或直接按函数名检索：
```bash
gf doc "make-enum-type"
```
