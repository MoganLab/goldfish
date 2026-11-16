# (liii pi ai config model-metadata)

模型规格元数据 `<pi-model-metadata>` 记录与序列化原语。

## 测试命令

```bash
bin/gf test library/tests/liii/pi/ai/config/model-metadata/
```

## 函数分类

- `pi-model-metadata` - 构造带默认匹配标志的 `<pi-model-metadata>` 记录
- `pi-model-metadata?` - 判断对象是否为 `<pi-model-metadata>` 记录
- `pi-model-metadata-id` - 获取模型标识符
- `pi-model-metadata-name` - 获取模型显示名称
- `pi-model-metadata-provider` - 获取所属服务商名称
- `pi-model-metadata-base-url` - 获取 API 基础地址
- `pi-model-metadata-context-window` - 获取上下文窗口 token 数
- `pi-model-metadata-max-tokens` - 获取单次输出最大 token 数
- `pi-model-metadata-matched?` - 获取是否匹配上内置模型表
- `pi-model-metadata->json` - 将 `<pi-model-metadata>` 转为 JSON 关联列表
