# (liii goldtool parallel) 模块函数分类索引

`(liii goldtool parallel)` 提供 Goldfish 工具链共享的并发设施：基于 `(liii par)`
的 worker 池封装，用于按文件并发执行检查/格式化任务，并统一结果表结构
`(路径 状态 失败信息)`。

使用时导入：

```scheme
(import (liii goldtool parallel))
```

## 常见用法示例

```scheme
;; 示例1：并发格式化一批文件
(pool-for-each
  (lambda (file) (list file 'ok "formatted"))
  files
  (lambda (file result) (display result))
) ;pool-for-each

;; 示例2：并发过滤出需要格式化的文件
(pool-filter (lambda (file) (needs-format? file)) files)

;; 示例3：统计失败数量
(count-status 'failed results)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldtool/parallel "函数名"
```

## 函数分类索引

### 一、并发度

- `goldtool-jobs` - 读取当前的文件级并发 worker 数量
- `set-goldtool-jobs!` - 设置文件级并发 worker 数量

### 二、任务执行

- `run-one-task` - 执行单个文件的任务并包装结果
- `pool-for-each` - 并发执行文件级任务并按序回调
- `pool-filter` - 并发过滤文件列表

### 三、结果统计

- `count-status` - 统计处于指定状态的结果数量
- `offenders-from` - 提取未通过的文件路径
