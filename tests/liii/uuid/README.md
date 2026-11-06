# (liii uuid) 模块函数分类索引

`(liii uuid)` 提供 UUID v4 随机标识符和 UUID v7 时间有序标识符的生成功能，适合对象标识、请求 ID、会话 ID 和数据库主键。

## 查看函数文档

```bash
gf doc liii/uuid
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/uuid "uuid7"
```

## 函数分类索引

- `uuid4` - 生成符合 RFC 9562 的随机 UUID v4 字符串
- `uuid7` - 生成包含 Unix 毫秒时间戳的时间有序 UUID v7 字符串
