package main

import (
	"fmt"
	"runtime"
	"time"
)

// seqFib 纯 CPU 密集型朴素递归计算（无缓存）
func seqFib(n int) int64 {
	if n <= 1 {
		return int64(n)
	}
	return seqFib(n-1) + seqFib(n-2)
}

// expandTree 将 Fibonacci 树展开指定深度，生成并发叶子任务列表
func expandTree(n int, depth int) []int {
	if depth <= 0 || n <= 1 {
		return []int{n}
	}
	left := expandTree(n-1, depth-1)
	right := expandTree(n-2, depth-1)
	return append(left, right...)
}

// parallelFib 基于 Go Channel 的树形分治并行算法
func parallelFib(n int, depth int) int64 {
	leaves := expandTree(n, depth)
	taskCount := len(leaves)
	ch := make(chan int64, taskCount)

	// 将各个叶子节点分发到独立 goroutine 并行计算
	for _, subN := range leaves {
		go func(sn int) {
			ch <- seqFib(sn)
		}(subN)
	}

	// 收集并累加所有叶子结果
	var sum int64 = 0
	for i := 0; i < taskCount; i++ {
		sum += <-ch
	}
	return sum
}

func main() {
	workers := runtime.NumCPU()

	fmt.Println("========================================================")
	fmt.Println("  Golang 原生 2分钟多核挑战：能算到 fib 几？")
	fmt.Println("  算法：基于 Go Channel 的树形分治并行 Fibonacci")
	fmt.Println("========================================================\n")

	fmt.Printf("检测到可用 CPU 工作核心数: %d\n", workers)

	const targetDuration = 120 * time.Second
	fmt.Printf("挑战目标时长: 120 秒 (2 分钟)\n")
	fmt.Println("并发机制：每阶 Fibonacci 树形展开为 32 个 Channel 并发叶子任务，各核心全负荷分治攻坚！")
	fmt.Println("请在另一个终端执行 htop 查看 CPU 占用（整整 2 分钟全部打满 2000%）！\n")

	startTime := time.Now()
	deadline := startTime.Add(targetDuration)

	// 从 35 阶开始冲刺（展开 5 层 = 32 个并发子任务）
	currentN := 35
	const depth = 5
	maxCompletedN := 0
	var maxCompletedVal int64 = 0

	fmt.Println("多核分治引擎已全线开火，开始冲击更高阶 Fibonacci...\n")

	for time.Now().Before(deadline) {
		t0 := time.Now()
		ans := parallelFib(currentN, depth)
		t1 := time.Now()

		cost := t1.Sub(t0).Seconds()
		totalElapsed := t1.Sub(startTime).Seconds()
		remSec := int(targetDuration.Seconds() - totalElapsed)
		if remSec < 0 {
			remSec = 0
		}

		maxCompletedN = currentN
		maxCompletedVal = ans

		fmt.Printf("  [剩余 %ds | 耗时 %.4fs] 攻克 fib(%d) = %d\n", remSec, cost, currentN, ans)

		currentN++
	}

	fmt.Println("\n2 分钟挑战时间到！正在生成统计战报...")

	totalSec := time.Since(startTime).Seconds()

	fmt.Println("\n========================================================")
	fmt.Println("  2 分钟多核极限冲刺完成！")
	fmt.Printf("实际总运行时间: %.2f 秒\n", totalSec)
	fmt.Printf("利用 %d 个核心，2 分钟内成功攻克的最大阶数是:\n\n", workers)
	fmt.Printf("    >>>  fib(%d) = %d  <<<\n\n", maxCompletedN, maxCompletedVal)
	fmt.Println("========================================================")
}
