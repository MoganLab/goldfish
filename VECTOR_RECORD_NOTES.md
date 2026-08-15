# Vector Based Record 实现与性能记录

## 状态
- vector based record 已提交（`85900c24`）：kernel 全部 record（`<syntax>`/`<context>`/`<store>`/`<bind>` 等）改为 vector 布局（let?=#f），根治 s7 inlet record 被 `map`/`let->list` 遍历的泄漏。
- GOLDFISH_BOOTSTRAP 自举完整、scheme/base 全量 212 测试通过。

## 性能差异
- 基准（my-or x2000，release）：
  - vector record：约 9.8-10.3s
  - inlet 基线：约 8.8-8.9s
  - 差距：慢约 12%（CPI 1.08 vs 1.00，cache-miss +12%）

## 根因（s7 未优化的固有特性）
- s7 的 vector 元素数组是**独立 mallocate 内存块**（每个 vector 占 2 个内存块：cell + 元素数组），而 inlet 的 slot 在 cell 池内连续；真实混合负载下 vector 数组访问 cache 不友好。
- 单操作基准反而更快：构造（make-syntax 1e6 1.47 vs 1.50s）、2M 存活构造（6.22 vs 6.57s）、accessor、谓词均占优。
- 合成基准无法复现该 cache 特性，仅在真实展开负载下显现。

## 已尝试无效的优化
- stx-vector? 检查简化（会导致 record 误判、死循环）
- predicate 内联简化（去掉 bytevector 检查，收益有限）
- 扩大堆 16M（反而更慢）
- make-fresh-name 计数器 vs gensym（性能相同）

## 后续性能提升方向（在其他地方寻找）
- s7 层面（待评估，风险高）：小 vector 元素内嵌 cell、优化 mallocate 分配器。
- expander 层面：减少展开路径的临时 `<syntax>` 构造/缓存；模板预编译进一步优化。
- 其他：profile 热点（GC 标记 mark_vector_1 为主）寻找可优化点。
