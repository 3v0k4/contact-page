---
title: Comparing any to nil in Golang
description:
author: Riccardo
tags:
  - Go
---

```go
package main

import (
	"fmt"
)

func main() {
	var a *int = nil

	var b any = nil
	fmt.Printf("b=%T\n", b) // b=<nil>
	b = a

	fmt.Printf("a=%T\n", a) // a=*int
	fmt.Printf("b=%T\n", b) // b=*int

	fmt.Printf("b=%v\n", a == b)   // true [1]
	fmt.Printf("b=%v\n", a == nil) // true [2]
	fmt.Printf("b=%v\n", b == nil) // false [3]

	var n *int = nil
	fmt.Printf("b=%v\n", b == n) // true
}

// [1]: typeOf(a) == typeOf(b)   && valueOf(a) == valueOf(b)
//      *int      == *int        && nil        == nil

// [2]: typeOf(a) == typeOf(nil) && valueOf(a) == valueOf(nil)
//      *int      == *int        && nil        == nil
//      the compiler casts nil to *nil because it knows the type of a

// [3]: typeOf(b) == typeOf(nil) && valueOf(b) == valueOf(nil)
//      *int      == <nil>       && nil        == nil
//      the compiler casts nil to <nil> because it knows the type of b
```
