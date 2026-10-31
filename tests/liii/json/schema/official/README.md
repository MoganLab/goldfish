# JSON Schema 官方测试套件

本目录保存离线运行所需的 JSON Schema Test Suite 数据，当前仅包含 `draft2020-12`。

- 上游：<https://github.com/json-schema-org/JSON-Schema-Test-Suite>
- 数据获取来源：`python-jsonschema/jsonschema` 仓库的 `json/tests/draft2020-12`
- 固定参考提交：`c497561ea0a95f5aac09cd63d15eb1420eb7bbec`
- 数据最后变更提交：`bb947f1e2cb4447da0647d79cbcb33f791cddd87`

更新数据时必须同时更新上述版本并审查差异，不得修改官方断言来制造通过。

```bash
gf test tests/liii/json/schema/official/run.scm
```

运行器输出 passed、failed、skipped 计数。尚未接入的分组属于 skipped，不计入 passed；缺失文件或执行异常属于失败。

## 当前基线（任务 042，发布验收）

- passed=1774，failed=0，skipped=271。
- 核心分组全部启用并通过（vocabulary 除外）。
- optional 分组启用并通过：anchor、bignum、dynamicRef、float-overflow、id、no-schema、non-bmp-regex、refOfUnknownKeyword、unknownKeyword。
- 格式分组启用并通过 12 个：date、date-time、duration、email、idn-email、ipv4、ipv6、json-pointer、regex、relative-json-pointer、time、uri、uri-reference。
- 保持 skipped（共 271 例，均为已文档化偏差，见 docs/deviations.md）：vocabulary（5，$vocabulary 动态方言未实现）、cross-draft（1）与 dependencies-compatibility（36，跨草稿按设计不支持）、ecmascript-regex（74，PCRE 后端方言差异）、format-assertion（4，格式默认不断言）、hostname（61）与 idn-hostname（90，IDNA2008 未实现）。

### 远程资源夹具（任务 042 补齐）

`tests/liii/json/schema/official/remotes/draft2020-12/` 保存官方套件引用的远程 Schema 副本，来源固定：

- 上游：JSON-Schema-Test-Suite `remotes/draft2020-12/`。
- 获取来源：`python-jsonschema/jsonschema` 仓库 `json/remotes/draft2020-12/`，参考提交 `c497561`（与 spec.md 的参考基线一致）。
- 包含子目录：baseUriChange、baseUriChangeFolder、baseUriChangeFolderInSubschema、nested。

运行器通过 `retrieve` 选项读取这些文件，不进行网络 I/O；`remote-file-name` 按 `http://localhost:1234/draft2020-12/` 前缀映射，无扩展名引用自动补 `.json`。

### 远程资源夹具（任务 031）

`tests/liii/json/schema/official/remotes/draft2020-12/` 保存官方套件引用的远程 Schema 副本。运行器通过 `retrieve` 选项读取这些文件，不进行网络 I/O。

- 已提供：`tree.json`、`extendible-dynamic-ref.json`、`detached-dynamicref.json`。
- 其余远程资源按任务 022、028、034 的需要逐步补齐。

### 可选测试（任务 022）

- 已执行并通过：bignum、float-overflow、non-bmp-regex。
- id、unknownKeyword：等待 `$id`、外部 Registry 和引用上下文（任务 025～028）。
- ecmascript-regex：PCRE 后端并非完整 ECMA-262，暂列后端差异。
- cross-draft、refOfUnknownKeyword：对应功能尚未实现。
- optional/dynamicRef：随任务 031 一并启用，依赖跨资源动态作用域。
- format-assertion：等待格式框架，format/ 子目录在任务 035～038 单独接入。
