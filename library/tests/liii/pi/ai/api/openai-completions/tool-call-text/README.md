# (liii pi ai api openai-completions tool-call-text) 模块函数索引

`(liii pi ai api openai-completions tool-call-text)` 负责把"模型把工具调用写在正文里"的
输出还原成标准工具调用：支持 XML 标签形式与 JSON 片段形式，供
`(liii pi ai api openai-completions stream-parse)` 在流结束时补齐 toolCall 块。

使用时导入：

```scheme
(import (liii pi ai api openai-completions tool-call-text))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc parse-openai-text-tool-call
```

## 函数分类索引

### 片段截取与解析
- `extract-openai-json-snippet` - 从模型正文里截取可能的 JSON 片段
- `parse-openai-xml-tool-call` - 从模型正文里解析 XML 形式的工具调用
- `parse-openai-text-tool-call` - 从模型正文里解析工具调用（优先 XML，其次 JSON）

### 流结束补齐
- `maybe-extract-openai-text-tool-call!` - 流结束时从正文里补齐内联工具调用

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/tool-call-text/
```

## 测试文件索引

- `extract-openai-json-snippet-test.scm` — `extract-openai-json-snippet`：JSON 片段截取
- `parse-openai-xml-tool-call-test.scm` — `parse-openai-xml-tool-call`：XML 形式解析
- `parse-openai-text-tool-call-test.scm` — `parse-openai-text-tool-call`：XML / JSON 统一解析
- `maybe-extract-openai-text-tool-call-bang-test.scm` — `maybe-extract-openai-text-tool-call!`：流结束补齐
