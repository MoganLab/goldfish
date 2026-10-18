# [ddd] 任务名

> 本文件是任务文档模板：执行任务 ddd 时复制为 `tools/pi/devel/ddd.md`（ddd 换成
> PI_PLAN.md §3 任务表的实际三位编号）后填写。编号与 PI_PLAN.md §3 一一对应；
> 一个任务编号对应一个代码 commit，本任务文档随该 commit 一同入库。

## 状态

待执行 / 进行中 / 已完成 / 已回退（注明原因与重试次数）

## 任务相关的代码文件

- （列出本任务涉及/新增的文件）

## 验收点

（从 PI_PLAN.md §3 任务表复制本任务的验收点，逐条可验证）

## 如何测试

```bash
# 完整的测试命令序列：构建 → fmt → diff → check → 本任务相关测试
timeout 600 xmake b goldfish
timeout 120 bin/gf fmt
git diff --exit-code
timeout 120 bin/gf check
timeout 300 bin/gf test tools/pi/tests/liii/<相关测试>.scm
```

## 日期 任务描述

### What

（做了什么、如何验证；编号列表）

### Why

（任务背景，可省略）

### How

（实现思路与难点，可省略）

## 验收脚本（REVIEW 阶段产物 ddd.scm 的写法）

- Scheme 脚本，`bin/gf tools/pi/devel/ddd.scm` 直接运行。
- 外部命令一律用 `(liii subprocess)` 的 `run` 执行，**必带 `:timeout`**；
  每步用 `(liii check)` 断言退出码与关键输出。

## 提交清单

- `[ddd] 任务名：简述`（commit hash）

## 执行与验收记录（Claude 维护）

- 日期 第 N 次拉起：结果（完成/失败/终止），要点
- 日期 验收结论：通过/失败，验证依据（哪些验收点过了/没过）
