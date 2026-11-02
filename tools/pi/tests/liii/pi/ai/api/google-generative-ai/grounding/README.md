# (liii pi ai api google-generative-ai grounding) 模块函数索引

`(liii pi ai api google-generative-ai grounding)` 负责 Gemini 的 Google Search
接地元数据处理与搜索工具注入：向请求体注入 googleSearch 工具节，
并从响应元数据中提取搜索查询、搜索结果与引用片段。

解析逻辑由 `(liii pi ai api google-generative-ai grounding-parse)` 提供。

使用时导入：

```scheme
(import (liii pi ai api google-generative-ai grounding))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc gemini-extract-grounding-details
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/google-generative-ai/grounding "gemini-extract-grounding-details"
```

## 函数分类索引

### 搜索工具注入
- `gemini-build-search-tools` - 构造包含 googleSearch 节的 Gemini tools 配置向量
- `gemini-inject-google-search` - 向 Gemini 请求体或已有的 tools 列表中注入 googleSearch 节

### 引用与元数据提取
- `gemini-extract-grounding-details` - 从 Gemini 响应中提取搜索查询、搜索结果和引用信息
- `gemini-grounding-search-queries` - 从提取出的 groundingDetails 或原始响应中读取搜索关键词列表
- `gemini-grounding-search-results` - 从提取出的 groundingDetails 或原始响应中读取网页搜索结果列表
- `gemini-grounding-citations` - 从提取出的 groundingDetails 或原始响应中读取引用片段列表

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/google-generative-ai/grounding/
```

## 测试文件索引

- `gemini-build-search-tools-test.scm` — `gemini-build-search-tools`：搜索工具节构造
- `gemini-inject-google-search-test.scm` — `gemini-inject-google-search`：请求体工具注入
- `gemini-extract-grounding-details-test.scm` — `gemini-extract-grounding-details`：真实 fixture 端到端提取
- `gemini-grounding-search-queries-test.scm` — `gemini-grounding-search-queries`：搜索查询读取
- `gemini-grounding-search-results-test.scm` — `gemini-grounding-search-results`：网页结果读取
- `gemini-grounding-citations-test.scm` — `gemini-grounding-citations`：引用片段读取
