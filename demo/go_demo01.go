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

func producer(ch chan<- int) {
	defer close(ch)
	for i := 1; i <= 5; i++ {
		ch <- i
	}
}

func consumer(ch <-chan int) {
	for v := range ch {
		fmt.Println("consume:", v)
	}
}

func main() {
	ch := make(chan int) // 无缓冲 channel
	go producer(ch)
	consumer(ch)
}
