# (liii pi code core tools) 模块函数分类索引

内置工具集。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 导出函数

- `PI_DEFAULT_TOOL_NAMES`
- `PI_ALL_TOOL_NAMES`
- `parse-tools-selection`
- `exclude-tools-from-list`
- `get-tool-definition`
- `execute-tool-call`
- `pi-read-schema`
- `pi-execute-read`
- `pi-write-schema`
- `pi-execute-write`
- `pi-edit-schema`
- `pi-execute-edit`
- `pi-bash-schema`
- `pi-execute-bash`
- `pi-ls-schema`
- `pi-execute-ls`
- `pi-find-schema`
- `pi-execute-find`
- `pi-grep-schema`
- `pi-execute-grep`
- `pi-format-size`
- `pi-truncate-line`
- `pi-truncate-head`
- `pi-truncate-tail`
- `PI_DEFAULT_MAX_LINES`
- `PI_DEFAULT_MAX_BYTES`
- `PI_GREP_MAX_LINE_LENGTH`
- `pi-tool-definition`
- `pi-tool-definition?`
- `pi-tool-definition-name`
- `pi-tool-definition-description`
- `pi-tool-definition-parameters`
- `pi-tool-definition-execute`
- `pi-tool-definition-snippet`
- `pi-tool-definition-guidelines`
- `pi-tool-definition->alist`
- `pi-tool-result`
- `pi-tool-result?`
- `pi-tool-result-content`
- `pi-tool-result-is-error`
- `pi-tool-result-isError`
- `pi-tool-result->alist`
- `pi-trunc-result`
- `pi-trunc-result?`
- `pi-trunc-result-content`
- `pi-trunc-result-truncated`
- `pi-trunc-result-truncated?`
- `pi-trunc-result-truncated-by`
- `pi-trunc-result-total-lines`
- `pi-trunc-result-total-bytes`
- `pi-trunc-result-output-lines`
- `pi-trunc-result-output-bytes`
- `pi-trunc-result-first-line-exceeds-limit?`
- `pi-trunc-result-first-line-truncated`
- `pi-trunc-result-last-line-partial?`
- `pi-trunc-result-last-line-truncated`
- `pi-trunc-result->alist`
