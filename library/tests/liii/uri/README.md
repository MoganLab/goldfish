# (liii uri) URI 处理库

`(liii uri)` 是基于 RFC 3986 的 URI 处理统一入口，聚合 URI 解析、构造、组件访问、修改、比较与字符串转换能力。

使用时导入：

```scheme
(import (liii uri))
```

## 常见用法示例

```scheme
(define u
  (string->uri "https://user:pass@api.example.com:8443/v1/users?id=123#profile"))

(uri-host u)
(uri-port u)
(uri-query-ref u "id")

(define next-page
  (uri-extend-query (uri-with-scheme u "https") '(("page" . "2"))))
(uri->string next-page)
```

## URI 的组成部分

根据 RFC 3986 规范，通用 URI 的结构定义为：

```text
URI = scheme:[//authority]path[?query][#fragment]
authority = [userinfo@]host[:port]
```

以完整 URI 示例说明各组成部分及在库中的对应访问器：

```text
  https://user:pass@api.example.com:8443/v1/users?id=123#profile
  \___/   \_______/ \_____________/ \__/ \______/ \_____/ \_____/
 scheme   userinfo       host       port   path    query  fragment
          \____________________________/
                    authority
```

- **scheme**（协议方案）：如 `https`、`http`、`file`，通过 `(uri-scheme u)` 获取
- **authority**（授权机构）：包含用户信息、主机与端口，通过 `(uri-authority u)` 获取
  - **user** / **password**（用户信息）：认证用户名与密码，通过 `(uri-user u)`、`(uri-password u)` 获取
  - **host**（主机）：域名或 IP 地址，通过 `(uri-host u)` 获取
  - **port**（端口）：端口号，通过 `(uri-port u)` 获取（未显式指定时自动回退对应 scheme 的标准端口），`(uri-explicit-port u)` 仅获取显式端口
- **path**（路径）：分层路径信息，通过 `(uri-path u)` 获取字符串，或通过 `(uri-path->list u)` 获取路径段列表；支持 `(uri-parent u)`、`(uri-name u)`、`(uri-suffix u)` 处理路径层级与后缀
- **query**（查询参数）：键值对映射，通过 `(uri-query u)` 获取 alist，`(uri-query-string u)` 获取原始字符串，或通过 `(uri-query-ref u key)` 按键查询
- **fragment**（片段标识符）：锚点信息，通过 `(uri-fragment u)` 获取

## 子模块

本库按职责拆分为多个子库，用户通常只需 `(import (liii uri))`：

- 对外聚合子库：
  - `(liii uri record)` - 构造与组件访问器、路径与查询参数处理、编码与解码
  - `(liii uri predicate)` - URI 与网络协议谓词
  - `(liii uri compare)` - URI 比较与哈希函数
  - `(liii uri make)` - URI 构造器
  - `(liii uri transform)` - URI 修改、路径合并与归一化
  - `(liii uri convert)` - URI 字符串转换
- 内部实现模块：`(liii uri internal parse)`

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/uri "函数名"
```

## 函数分类索引

### 构造 URI
- `make-uri`、`string->uri` - 从字符串解析 URI
- `make-uri-record` - 从原始组件创建 URI 记录
- `uri-build` - 使用关键字参数构造 URI

### 访问 URI 组件
- `uri-scheme`、`uri-host`、`uri-port`、`uri-path` - 获取 URI 基本组件
- `uri-query`、`uri-query-ref`、`uri-query-ref*` - 获取查询参数
- `uri-fragment`、`uri-user`、`uri-password` - 获取片段与认证信息
- `uri-authority`、`uri-explicit-port` - 获取 authority 与显式端口

### 修改 URI
- `uri-with-scheme`、`uri-with-host`、`uri-with-port` - 替换协议、主机或端口
- `uri-with-path`、`uri-with-fragment` - 替换路径或片段
- `uri-update-query`、`uri-extend-query` - 更新或扩展查询参数
- `uri-without-query`、`uri-without-query-param` - 移除查询参数
- `uri-join-path`、`uri-join` - 追加路径段或合并 URI
- `uri-with-name`、`uri-with-suffix` - 修改路径末级名称或后缀
- `uri-normalize` - 归一化 URI

### 转换、比较与编码
- `uri->string` - URI 转为完整字符串
- `uri->human-string` - 生成隐藏敏感信息的字符串
- `uri=?`、`uri<?`、`uri>?` - URI 相等与字典序比较
- `uri-hash` - 计算 URI 哈希值
- `uri-encode`、`uri-decode` - 百分比编码与解码
- `uri-encode-path`、`uri-decode-path` - 路径编码与解码
- `query-string->alist`、`alist->query-string` - 查询字符串与 alist 互转

### 谓词
- `uri?` - 检查对象是否为 URI 记录
- `uri-absolute?`、`uri-relative?` - 检查绝对或相对 URI
- `uri-default-port?` - 检查是否使用默认端口
- `uri-network-scheme?` - 检查是否为网络协议
