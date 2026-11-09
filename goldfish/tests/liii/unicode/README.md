# (liii unicode) 模块函数分类索引

`(liii unicode)` 提供 UTF-8、UTF-16 与 Unicode 码点之间的
转换与处理能力，适合编码转换、协议编解码、
字符工具和底层文本处理逻辑。

## 常见用法示例

```scheme
(import (liii unicode))

(utf8-string-length "Hello 你好")   ; => 8（按码点计数）
(utf8-string-trim-both "  abc  ")  ; => "abc"
(codepoint->utf8 #x4e2d)            ; => #u8(228 184 173)
(utf8->string #u8(240 159 145 141)) ; => "👍"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/unicode "函数名"
```

## 函数分类索引

### 一、UTF-8 字符串
- `utf8-string` / `utf8-make-string` / `utf8-string-set!` - 构造与修改
- `utf8-string-length` / `utf8-substring` - 按码点定位与截取
- `utf8-string-trim-left` / `utf8-string-trim-right` / `utf8-string-trim-both` - 去除空白

### 二、UTF-8 编解码
- `utf8->string` / `string->utf8` - 与字符串互转（scheme base）
- `utf8->codepoint` / `utf8->codepoint-at` / `codepoint->utf8` - 码点编解码
- `bytevector-advance-utf8` / `bytevector-advance-utf8-strict` - 字节向量前进
- `utf8-sequence-length` / `utf8-replace-invalid` - 序列长度与非法序列替换

### 三、UTF-16 转换
- `utf8->utf16le` / `utf8->utf16be` - UTF-8 转 UTF-16
- `utf16le->utf8` / `utf16be->utf8` - UTF-16 转 UTF-8
- `utf16le->codepoint` / `utf16be->codepoint` - UTF-16 解码码点
- `codepoint-to-utf16le` / `codepoint-to-utf16be` - 码点编码 UTF-16
- `bytevector-utf16le-advance` / `bytevector-utf16be-advance` - UTF-16 前进

### 四、码点与十六进制
- `codepoint->hexstr` / `hexstr->codepoint` - 码点十六进制表示

### 五、常量
- `unicode-max-codepoint` - Unicode 最大有效码点
- `unicode-replacement-char` - Unicode 替换字符

## 错误处理约定

- 参数类型不符时抛出 `type-error`
- 无效的 UTF-8/UTF-16 序列抛出 `value-error`
- 索引越界抛出 `out-of-range`
