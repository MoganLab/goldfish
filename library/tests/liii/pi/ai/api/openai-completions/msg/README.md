# (liii pi ai api openai-completions msg) 模块函数索引

`(liii pi ai api openai-completions msg)` 负责 OpenAI 协议请求体与消息转换：
将内部会话历史转为 OpenAI messages 向量、把工具定义转换为 function calling
格式，并拼装完整的请求体 alist。

使用时导入：

```scheme
(import (liii pi ai api openai-completions msg))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc openai-build-messages
```

或者指定库名：

```bash
gf doc liii/pi/ai/api/openai-completions/msg "openai-build-messages"
```

## 函数分类索引

### 请求消息与工具
- `openai-build-tool-declarations` - 将内部 tool-definition records 转换为 OpenAI function calling 格式
- `openai-build-messages` - 将内部消息历史转换为 OpenAI Chat Completions messages
- `openai-build-request-body` - 组装 OpenAI Chat Completions 请求体 alist

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/ai/api/openai-completions/msg/
```

## 测试文件索引

- `openai-build-tool-declarations-test.scm` — `openai-build-tool-declarations`：工具声明格式转换
- `openai-build-messages-test.scm` — `openai-build-messages`：消息角色映射与边界过滤
- `openai-build-request-body-test.scm` — `openai-build-request-body`：请求体组装
