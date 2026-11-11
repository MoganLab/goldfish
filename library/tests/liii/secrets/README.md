# (liii secrets) 模块函数分类索引

`(liii secrets)` 提供密码学安全随机数（CSPRNG）相关的高层工具，
对齐 Python PEP 506（Adding A Secrets Module To The Standard Library）的设计：
底层熵源由 `(liii os)` 的 `urandom` 提供，本库只负责 Token 编码与业务安全工具。

> 需要**可复现**的伪随机序列时，请使用 `(liii random)` 或 `(srfi srfi-27)`，
> 不要用本库代替伪随机数生成器。

## 查看函数文档

```bash
gf doc liii/secrets
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/secrets "token-urlsafe"
gf doc liii/secrets "compare-digest"
```

## 函数分类索引

### 一、安全 Token 生成
- `token-bytes` - 生成指定长度的密码学安全随机字节向量（默认 32 字节）
- `token-hex` - 生成 `2 * nbytes` 长度的十六进制小写随机字符串，适合 CSRF Token 与防重放 Nonce
- `token-urlsafe` - 生成不含 `=` 填充的 URL-Safe Base64 随机字符串，天然满足 RFC 7636 PKCE `code_verifier` 规范

### 二、无偏随机取样
- `randbelow` - 在 `[0, n)` 区间内产生无模偏差的均匀随机整数（拒绝采样）
- `choice` - 从非空列表或向量中密码学安全地随机选取单一元素

### 三、机密性比对
- `compare-digest` - 常量时间比对两个字符串或两个 bytevector，防范时序旁路攻击

## 相关模块

- `(liii os)` - 提供底层系统熵源 `urandom`
- `(liii base64)` - 提供 `token-urlsafe` 依赖的 Base64 编码能力
