# (tools pi extensions) 内置扩展脚本集成测试

`tools/pi/tests/liii/pi/extensions/` 提供 Pi 内置扩展脚本（code-review、simplify、web-search）的加载与注册验证测试。

使用时加载扩展：

```scheme
(import (liii pi code core extensions))
(pi-load-extension "tools/pi/extensions/code-review.scm" api)
```

## 测试用例列表

- `code-review-test.scm` - 验证 code-review 扩展的 flags、commands、flows 与 validators 注册
- `simplify-test.scm` - 验证 simplify 扩展的 flags、commands 与 flows 注册
- `web-search-test.scm` - 验证 web-search 扩展的 web_search 工具注册
