# (liii json schema model) 模块函数分类索引

`(liii json schema model)` 定义 JSON Schema 校验器的数据模型：
校验器记录与资源登记表记录。

两个记录都是不可变的 record，字段通过访问器读取；
校验过程中的可变状态不保存在记录里。

## 常见用法示例

```scheme
(import (liii json schema model))

(define validator (make-validator '(("type" . "integer")) '()))
(validator-schema validator)   ; => (("type" . "integer"))
(validator-options validator)   ; => ()

(define registry (make-registry '()))
(registry-resources registry)   ; => ()
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/model "函数名"
```

## 函数分类索引

### 一、校验器记录
- `make-validator` - 创建校验器记录
- `json-schema-validator?` - 判断对象是否为校验器记录
- `validator-schema` - 读取校验器记录中的根 Schema
- `validator-options` - 读取校验器记录中的选项

### 二、资源登记表记录
- `make-registry` - 创建资源登记表记录
- `json-schema-registry?` - 判断对象是否为资源登记表记录
- `registry-resources` - 读取资源登记表记录中的资源列表

## 错误处理约定

- 记录构造函数不校验入参，非法入参由上层负责
- 两个谓词只识别本模块创建的记录
