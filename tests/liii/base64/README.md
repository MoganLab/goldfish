# (liii base64) 模块函数分类索引

`(liii base64)` 提供字符串和字节向量的 Base64 编解码能力，适合处理二进制载荷、文本协议和序列化结果。

## 查看函数文档

```bash
gf doc liii/base64
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/base64 "base64-encode"
```

## 函数分类索引

### 一、统一入口
- `base64-encode` - 根据输入类型执行 Base64 编码
- `base64-decode` - 根据输入类型执行 Base64 解码

### 二、字符串编解码
- `string-base64-encode` - 将字符串的 UTF-8 字节编码为 Base64 字符串
- `string-base64-decode` - 将 Base64 字符串解码为 UTF-8 字符串

### 三、字节向量编解码
- `bytevector-base64-encode` - 将字节向量编码为 Base64 字节向量
- `bytevector-base64-decode` - 将 Base64 字节向量解码为原始字节向量
