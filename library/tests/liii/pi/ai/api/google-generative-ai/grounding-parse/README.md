# (liii pi ai api google-generative-ai grounding-parse) 模块函数索引

`(liii pi ai api google-generative-ai grounding-parse)` 负责从 Gemini 响应中
提取 Google Search 接地元数据：解析候选中的 `groundingMetadata`，
提取搜索关键词、带 URL 的网页搜索结果以及关联文本片段的引用信息。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai grounding-parse))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-extract-grounding-details
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/grounding-parse "gemini-extract-grounding-details"
```

## 函数分类索引

### 接地元数据提取
- `gemini-extract-grounding-details` - 从 Gemini 响应中提取搜索查询、搜索结果和引用信息
- `gemini-grounding-search-queries` - 从提取出的 groundingDetails 或原始响应中读取搜索关键词列表
- `gemini-grounding-search-results` - 从提取出的 groundingDetails 或原始响应中读取网页搜索结果列表
- `gemini-grounding-citations` - 从提取出的 groundingDetails 或原始响应中读取引用片段列表

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/grounding-parse/
```

## 测试文件索引

- `gemini-extract-grounding-details-test.scm` — `gemini-extract-grounding-details`：真实 fixture 端到端提取
- `gemini-grounding-search-queries-test.scm` — `gemini-grounding-search-queries`：搜索查询读取
- `gemini-grounding-search-results-test.scm` — `gemini-grounding-search-results`：网页结果读取
- `gemini-grounding-citations-test.scm` — `gemini-grounding-citations`：引用片段读取
