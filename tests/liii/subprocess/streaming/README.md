# (liii subprocess streaming) 模块函数分类索引

`(liii subprocess streaming)` 提供子进程输出的流式处理能力：
逐行回调 stdout / stderr，支持超时强杀、空闲超时预警，
适合处理长时间运行、持续产生输出的外部命令。

## 常见用法示例

```scheme
(import (liii subprocess streaming))

;; 逐行收集输出
(define lines '())
(run-streaming '("git" "log" "--oneline")
  :on-stdout (lambda (line) (set! lines (cons line lines))))

;; 空闲超时保护
(run-streaming cmd :on-stdout handler :timeout 10 :idle-timeout 3)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/subprocess/streaming "函数名"
```

## 函数分类索引

### 一、流式执行
- `run-streaming` - 以流式回调方式执行命令并逐行处理子进程输出
- `run-streaming-chunks` - 以数据块为单位流式读取子进程输出

## 错误处理约定

- 回调参数不是过程时抛出 `type-error`
- 超时值为负数或 `idle-timeout >= timeout` 时抛出 `value-error`
- 超时或被强杀时返回 `#f`
