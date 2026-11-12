# (liii goldtest-runner-report) 模块函数分类索引

`gf test` 的测试报告渲染：失败日志回显、失败行打印、捕获输出尾部转储与
Summary 汇总统计（含 cached 命中数展示）。

使用时导入：

```scheme
(import (liii goldtest-runner-report))
```

## 常见用法示例

```scheme
;; 示例1：汇总测试结果并返回失败数
(define failed (display-summary (list (cons "a.scm" 0) (cons "b.scm" 1))))

;; 示例2：统计并行结果中的缓存命中数
(%count-cached-results (list (list "a.scm" 0 "cached")))
```

## 函数分类

### 缓存命中统计

- `cached-count-bump!` - 缓存命中计数自增
- `cached-count-reset!` - 缓存命中计数清零
- `%count-cached-results` - 从结果列表统计缓存命中数

### 汇总与清单

- `display-summary` - 输出 Summary 统计（含 cached 计数），返回失败数
- `failed-test-files` - 提取失败测试文件列表

### 失败诊断渲染

- `display-fail-log` - 回显失败日志文件内容
- `cleanup-log` - 删除临时日志文件
- `%report-test-failure` - 打印单文件失败行与捕获输出尾部
- `%dump-captured-tail` - 打印捕获文本的末尾若干行
