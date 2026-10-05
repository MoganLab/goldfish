# JSON 应用负载热点采样

使用仓库的 `(liii json)` 性能脚本作为应用型负载。它构造包含 200 个对象键和
200 个数组项的 JSON 文本，反复解析、序列化、查键、枚举键并检查类型；脚本含
预热并完整输出所有操作结果。运行命令：

```sh
GOLDFISH_CACHE_DIR=/tmp/gf-startup-cache/cache \
  ./bin/gf -m liii bench/json-perf.scm
```

本次采样使用 `perf` 7.1.6、`cpu-clock:u`、99 Hz 和 DWARF 调用栈。共采到
2,770 个样本，没有丢样。执行二进制的 SHA256、工作负载摘要、缓存版本和采样参数
记录在 `metadata.tsv`。`gf-symbols.gz` 是用当前 release objects 重链的符号副本；
其 `.text` 与 `.rodata` 均和实际执行的剥离二进制逐字节匹配。原始采样、stdout、
stderr 及报告均留在本目录。

## 观察

自身 CPU 样本主要落在 evaluator `run_machine`（12.31%）、`GC_free`（10.40%）、
GC mark（7.51%）、`Environment::lookup`（6.71%）、continuation frame 压入
（5.13%）和 `GC_malloc`（5.05%）。调用树显示 `Evaluator::proper_list` inclusive
约 9.9%，其中 `std::vector<Value>` 扩容路径约 8.2%；这些嵌套比例不能相加。

这是端到端脚本采样，覆盖启动后的源文件加载、库安装、预热和所有 JSON 子操作，
不能把这些比例解释成 JSON parser 或单个 primitive 的独占成本。热区仍集中在
此前出现过的通用 evaluator、环境访问和临时分配路径。`proper_list` 的预留容量
候选已在宽集合负载上做过 A/B，收益不稳定，故没有从本次高 inclusive 占比推断
应当保留该改动。GC 释放与标记也不应仅凭 CPU 占比而删减。

## 下一步

先把 JSON 负载拆成独立进程的 parse、stringify、lookup 和 key enumeration 场景，
各自保留相同输入和结果检查，再为 CPU 占比最高且有明确调用路径的一个场景设计
局部候选。这样可以区分脚本加载与 evaluator 的常驻成本，避免对通用环境查找或
continuation 表示做没有语义边界的猜测性改动。

`flat.txt` 是自身采样报告，`inclusive.txt` 和 `callgraph.txt` 是含调用者的报告；
嵌套 inclusive 百分比不能相加。运行的 Scheme 源文件位于
[`bench/json-perf.scm`](../json-perf.scm)。
