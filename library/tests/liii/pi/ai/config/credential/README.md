# (liii pi ai config credential)

服务商 API 凭据 `<pi-credential>` 记录与序列化原语。

## 测试命令

```bash
bin/gf test library/tests/liii/pi/ai/config/credential/
```

## 函数分类

- `pi-credential` - 构造带默认凭据类型的 `<pi-credential>` 记录
- `pi-credential?` - 判断对象是否为 `<pi-credential>` 记录
- `pi-credential-provider` - 获取服务商名称
- `pi-credential-key` - 获取 API 密钥
- `pi-credential-type` - 获取凭据类型
- `pi-credential->json` - 将 `<pi-credential>` 转为 JSON 关联列表
