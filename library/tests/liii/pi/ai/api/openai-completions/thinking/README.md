# (liii pi ai api openai-completions thinking) 测试导览

`(liii pi ai api openai-completions thinking)` 负责请求侧思考参数：
从 `models-store` 定位原始模型条目，并按 `thinkingFormat`、`thinkingLevelMap`
生成 `thinking` / `reasoning_effort` 字段。

## 运行测试

```bash
bin/gf test library/tests/liii/pi/ai/api/openai-completions/thinking/
```

## 测试文件索引

- `openai-resolve-model-entry-test.scm` — 从 models-store 定位模型条目
- `openai-thinking-request-params-test.scm` — deepseek/openai 两种格式的思考字段生成
