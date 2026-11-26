# (liii zlib) 模块函数分类索引

`(liii zlib)` 提供 gzip / deflate / 裸 DEFLATE 的一次性压缩与解压，
自动识别容器解压，以及 CRC-32 / Adler-32 校验和计算。语义对齐 Python
`zlib` 模块，适用于数据压缩、校验和、WebSocket permessage-deflate 上下文等。

## 常见用法示例

```scheme
(import (liii zlib) (liii unicode))

;; 压缩 / 解压往返（输出恒为 bytevector，文本需 utf8->string）
(utf8->string (gzip-decompress (gzip-compress "hello" 6)))   ; => "hello"

;; 自动识别 gzip / zlib 容器
(utf8->string (decompress (deflate-compress "hello" 6)))    ; => "hello"

;; 校验和（与 Python 一致：crc32 空=0，adler32 空=1）
(crc32 "123456789")          ; => #xCBF43926
(adler32 "123456789")        ; => #x091E01DE
(zlib-version)               ; => "1.3.2"
```

## 容器与 windowBits

| API | 容器格式 | windowBits |
| --- | --- | --- |
| `gzip-*` | gzip（RFC 1952，魔数 #x1F #x8B） | `wbits + 16` |
| `deflate-*` | zlib（RFC 1950，头标志 #x78） | `wbits` |
| `raw-deflate-*` | 裸 DEFLATE（RFC 1951，无头尾） | `-wbits` |
| `decompress` | auto：自动识别 gzip / zlib | `15 + 32` |

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/zlib "函数名"
```

## 函数分类索引

### 一、压缩
- `gzip-compress` - gzip 容器压缩
- `deflate-compress` - zlib 容器压缩（对齐 Python zlib.compress）
- `raw-deflate-compress` - 裸 DEFLATE 压缩（WS permessage-deflate 用）

### 二、解压
- `gzip-decompress` - gzip 容器解压，支持多成员拼接流
- `deflate-decompress` - zlib 容器解压
- `raw-deflate-decompress` - 裸 DEFLATE 解压
- `decompress` - 自动识别 gzip / zlib 容器解压

### 三、校验和
- `crc32` - 计算 CRC-32 校验和
- `adler32` - 计算 Adler-32 校验和

### 四、版本
- `zlib-version` - 返回链接的 zlib 版本字符串

## 错误处理约定

- 输入类型不符 → `type-error`
- level 非 -1/0..9、window-bits 非 8..15、mem-level 非 1..9、未知 strategy → `value-error`
- 损坏数据 / 容器格式不符 / 解压窗口过小 / 解压输出超 512MB → `zlib-error`
