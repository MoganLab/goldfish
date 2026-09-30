# 测试文件约定

## 目录

库入口文件介绍模块；具体断言放在模块目录中：

```text
tests/<group>/<library>-test.scm
tests/<group>/<library>/<function>-test.scm
```

例如，`tests/liii/hash-table-test.scm` 是模块导览，具体函数测试放在
`tests/liii/hash-table/`。测试资源放在 `tests/resources/`，不按过程命名。

## 模块导览

入口文件说明模块用途，提供少量示例、`gf doc` 用法和导出函数索引。不在入口文件中放 `check` 断言；断言放进对应的函数测试文件。

## 函数测试

每个文件围绕一个过程、谓词、构造器或语法入口组织。通常包括：

1. 导入 `(liii check)` 和被测库。
2. 设置 `(check-set-mode! 'report-failed)`。
3. 用简短注释说明语法、输入、返回值和错误行为。
4. 添加行为断言，并以 `(check-report)` 结束。

必要时可定义局部辅助函数和夹具；不要把无关 API 的断言混在同一文件。

## 文件名

测试文件名为 `<exported-name>-test.scm`，特殊字符按下表转换：

| 名称 | 文件名中的写法 |
| --- | --- |
| `?` | `-p` |
| `!` | `-bang` |
| `/` | `-slash-` |
| `->` | `-to-` |
| `*` | `-star` |
| `+`, `-`, `*`, `/` | `plus`, `minus`, `star`, `slash` |
| `=`, `<`, `<=`, `>`, `>=` | `eq`, `lt`, `le`, `gt`, `ge` |

例如：`string->list` 对应 `string-to-list-test.scm`，`vector=` 对应
`vector-eq-test.scm`，`hash-table-update!/default` 对应
`hash-table-update-bang-slash-default-test.scm`。

## 运行

```sh
./bin/gf test tests/liii/hash-table/hash-table-ref-test.scm
./bin/gf test tests/liii/hash-table/
```

全量测试使用 `./bin/gf test --all`；无参数行为和 native 测试策略见
`AGENTS.md`。
