# (liii pi ai message record) 模块函数索引

`(liii pi ai message record)` 提供面向业务的消息与内容块构造器、角色判定、类型提取及转换为 alist 的接口。

使用时导入：

```scheme
(import (liii pi ai message record))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-user-message
```

## 函数分类索引

### 消息构造与转换
- `pi-message?` - 判断是否为消息对象
- `pi-message-role` - 获取消息角色
- `pi-message->alist` - 消息对象转 alist
- `pi-user-message` - 构造用户消息
- `pi-user-message-role` - 获取用户消息角色
- `pi-assistant-message` - 构造助手消息
- `pi-assistant-message-role` - 获取助手消息角色
- `pi-tool-result-message` - 构造工具结果消息
- `pi-tool-result-message-role` - 获取工具结果消息角色

### 内容块构造与转换
- `pi-block?` - 判断是否为内容块
- `pi-block-type` - 获取内容块类型
- `pi-block->alist` - 内容块转 alist
- `pi-text-block` - 构造文本块
- `pi-text-block-type` - 获取文本块类型
- `pi-thinking-block` - 构造思考块
- `pi-thinking-block-type` - 获取思考块类型
- `pi-tool-call-block` - 构造工具调用块
- `pi-tool-call-block-type` - 获取工具调用块类型
- `pi-image-block` - 构造图片块
- `pi-image-block-type` - 获取图片块类型
- `pi-bash-execution->user-text` - 构造 Bash 执行结果注入文本

## 多模态图片块（Image Block）与大模型传输架构说明

### 1. 为什么大模型 API 必须使用 Base64 编码？

很多开发者的第一直觉是：“为什么不直接把图片文件的本地绝对路径（如 `/path/to/image.png`）传给模型，为什么非要转成体积膨胀约 33% 的 Base64 文本？”

- **云端服务无法访问本地文件**：大模型服务（OpenAI、Anthropic、Google Gemini 等）均部署在公有云端，与开发者的本地机器处于隔离网络。大模型推理集群无法直接读取用户本机磁盘路径；
- **JSON 协议的字符集限制**：大模型主流 API 是基于 HTTP/REST 的无状态 JSON 请求。标准 JSON 格式规定字符流必须是合法 UTF-8 文本，无法直接内嵌包含 `\x00` 等非打印字符的裸二进制字节流；
- **标准事实协议**：行业各大模型厂商统一采用 Base64 字符串作为 JSON 报文中内嵌图片数据的标准传输载荷（Data URL 或纯 Base64 字段）。

### 2. 各大模型协议请求体 JSON 组装规范

大模型协议对工具执行结果（`toolResult`）内嵌图片的处理机制存在差异，适配器层（API layer）在发送请求时做如下转换：

#### 2.1 OpenAI Chat Completions 协议
OpenAI 规定 `role: "tool"` 消息必须对应一个 `tool_call_id` 且 `content` 只能是纯字符串，不支持直接嵌套多模态内容。为向模型传递图片，上游标准方案为**拆分双消息**：
1. 先发送一条 `tool` 角色消息闭合调用 ID，内容为文本说明；
2. 紧接着生成一条 `user` 角色消息，通过 `image_url` 块携带 Data URL 附件：

```json
[
  {
    "role": "tool",
    "tool_call_id": "call_img_1",
    "content": "Read image file [image/png]"
  },
  {
    "role": "user",
    "content": [
      {
        "type": "image_url",
        "image_url": {
          "url": "data:image/png;base64,iVBORw0KGgo..."
        }
      }
    ]
  }
]
```

#### 2.2 Anthropic Messages 协议
Anthropic 原生支持在 `tool_result` 块内嵌套多模态内容。将内容块封装为包含 `type: "image"` 且 `source: base64` 的结构：

```json
{
  "role": "user",
  "content": [
    {
      "type": "tool_result",
      "tool_use_id": "call_img_1",
      "content": [
        { "type": "text", "text": "Read image file [image/png]" },
        {
          "type": "image",
          "source": {
            "type": "base64",
            "media_type": "image/png",
            "data": "iVBORw0KGgo..."
          }
        }
      ]
    }
  ]
}
```

#### 2.3 Google Gemini 协议
Gemini 支持在函数响应的 `parts` 中内嵌 `inlineData` 多模态结构：

```json
{
  "role": "user",
  "parts": [
    {
      "functionResponse": {
        "name": "read",
        "id": "call_img_1",
        "response": { "output": "Read image file [image/png]" },
        "parts": [
          {
            "inlineData": {
              "mimeType": "image/png",
              "data": "iVBORw0KGgo..."
            }
          }
        ]
      }
    }
  ]
}
```

### 3. 模型如何理解图片？Token 消耗机制

模型并不会把 Base64 当作长字符串逐字阅读，而是在 API 接入网关将 Base64 解码还原为像素张量，送入视觉编码器（Vision Encoder，如 ViT）：
- **网格分片与视觉 Token 映射**：模型通常按固定网格（如 512×512 像素为一个 tile）对图片进行切片；
- **固定 Token 换算**：每个 tile 转换为固定数量的视觉 token（如 OpenAI 低精度模式 85 tokens，高精度模式每个 tile 约 170 tokens；Claude/Gemini 类似按分辨率折算）；
- 因此，只要图片分辨率在合理范围内，大模型所消耗的上下文 Token 数量是完全可控的，并不会因为 Base64 字符串本身的字符数而引发上下文暴涨。

### 4. Agent 如何存储图片上下文与避免体积爆炸？

Base64 字符串占用大量字符空间，若每一轮对话都重复携带前序轮次的所有历史图片，会导致请求体急剧膨胀甚至突破上下文上限。Agent 采用分层治理策略：

1. **当前轮次内存保留**：
   - 工具执行完毕后，返回包含 `<pi-image-block>` 的多模态 `toolResult` 消息；
   - 在随后发起的大模型请求中，将图片发送给模型，供当前轮次视觉分析。
2. **非视觉模型自动降级过滤**：
   - 若当前模型不支持图片输入（元数据 `input` 字段不包含 `"image"`），工具层会追加提示 `"[Current model does not support images. The image will be omitted from this request.]"`，并在请求体构建阶段剔除图片块，防止向纯文本模型发送多模态字段导致 400 报错。
3. **会话压缩（Compaction）与长期落盘**：
   - **会话持久化**（`sessions/*.jsonl`）：事件流按行序列化保存真实会话记录；
   - **自动上下文压缩**：当会话总 token 数逼近模型的上下文窗口水位线时，Agent 触发压缩流程（compactor）；压缩算法将早期轮次的历史详细输出（包括冗长的图片 Base64 载荷）折叠摘要化，仅保留文本结论，丢弃巨大的 Base64 数据串，从根本上杜绝多轮对话中图片的无限累积。
