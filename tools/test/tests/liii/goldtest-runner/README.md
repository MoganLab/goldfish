# (liii goldtest-runner) 模块函数分类索引

gf test 测试执行器：串行与并行拉起测试子进程，含 tools 目录测试缓存接入。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 导出函数

- `ESC`
- `color`
- `GREEN`
- `RED`
- `YELLOW`
- `RESET`
- `goldfish-cmd`
- `run-test-file`
- `run-tests-parallel`
- `%parallel-result`
- `%run-one-parallel-test`
- `%build-indexed-vec`
- `%parallel-cached-result`
- `%process-exit-code`
- `%captured-stdout`
- `%captured-stderr`
- `%print-progress-marker`

### 缓存控制

- `*test-cache-force*`
- `set-test-cache-force!`
- `test-cache-force?`
- `%cache-check`
- `%cache-record`

### 报告渲染（转发自 (liii goldtest-runner-report)）

- `display-fail-log`
- `cleanup-log`
- `failed-test-files`
- `display-summary`
- `%dump-captured-tail`
- `%report-test-failure`
- `cached-count-bump!`
- `cached-count-reset!`
- `%count-cached-results`
