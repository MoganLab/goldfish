//
// Copyright (C) 2026 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

package main

import "fmt"

// stage1 流水线阶段 1：数据生成者
//
// 将数字 1 到 5 依次发送到输出通道 out 中。
// 发送完毕后通过 defer close(out) 关闭通道，向下游阶段发出数据流结束的信号。
func stage1(out chan<- int) {
	defer close(out)
	for i := 1; i <= 5; i++ {
		out <- i
	}
}

// stage2 流水线阶段 2：数据变换处理者（平方计算）
//
// 从输入通道 in 中持续消费数据，将每个数字进行平方计算后发送到输出通道 out 中。
// 当上游通道 in 关闭且数据排空后，for-range 循环自动退出，
// 随后通过 defer close(out) 关闭下游通道，将结束信号继续向后级传递。
func stage2(in <-chan int, out chan<- int) {
	defer close(out)
	for v := range in {
		out <- v * v
	}
}

// main 主函数：组装并启动并发流水线
func main() {
	// 1. 创建用于连接各个流水线阶段的通道
	nums := make(chan int)
	squares := make(chan int)

	// 2. 分别启动独立的 goroutine 并发执行各个流水线阶段：
	//    - stage1 在后台生成数据并写入 nums；
	//    - stage2 在后台从 nums 读取数据，计算平方后写入 squares。
	go stage1(nums)
	go stage2(nums, squares)

	// 3. 阶段 3：在主 goroutine 中作为最终消费者，消费并打印最终产物。
	//    当 stage2 处理完毕并 close(squares) 后，for-range 自动结束，程序正常退出。
	for v := range squares {
		fmt.Println("square:", v)
	}
}
