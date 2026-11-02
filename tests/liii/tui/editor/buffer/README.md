# (liii tui editor buffer) 模块函数索引

`(liii tui editor buffer)` 提供行编辑器的文本行缓冲区核心数据结构，支持单行/多行模式切换、字符与字符串插入、Unicode/CJK 全角字符退格与删除、多行折叠分裂以及行替换。

使用时导入：

```scheme
(import (liii tui editor buffer))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-buffer
```

## 函数分类索引

### 缓冲区创建与属性
- `make-buffer` - 创建编辑缓冲区实例
- `buffer?` - 判断是否为缓冲区实例
- `buffer-lines` - 获取行列表
- `buffer-lines-set!` - 设置行列表
- `buffer-row` - 获取当前行号
- `buffer-row-set!` - 设置当前行号
- `buffer-col` - 获取当前列号
- `buffer-col-set!` - 设置当前列号
- `buffer-multiline?` - 判断是否为多行模式
- `buffer-text` - 获取全文文本
- `buffer-set-text!` - 整体设置全文文本

### 文本编辑与变动
- `buffer-insert-char!` - 插入单字符
- `buffer-insert-string!` - 插入单行字符串
- `buffer-insert-text!` - 插入全文块（支持换行展开）
- `buffer-backspace!` - 向前退格删除
- `buffer-delete!` - 向后删除
- `buffer-newline!` - 插入换行分裂行
- `%replace-line` - 纯函数替换指定行
