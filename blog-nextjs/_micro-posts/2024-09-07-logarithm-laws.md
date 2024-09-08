---
title: Logarithm laws
author: Riccardo
description: A quick recap of some fundamental properties of logarithms
tags:
  - Math
---

## Power law

<code>log~b~p^n^ = n * log~b~p</code>

**Proof:**

<code>log~b~p = a</code> -> <code>b^a^ = p</code>

Raise to `k` -> <code>b<sup>k\*a</sup> = p^k^</code>

Apply <code>log~b~</code> -> <code>k * a = log~b~p^k^</code>

Replace `a` with <code>log~b~p</code> -> <code>k * log~b~p = log~b~p^k^</code>

## Product law

<code>log~b~m\*n = log~b~m + log~b~n</code>

**Proof:**

<code>log~b~m = i</code> -> <code>b^i^ = m</code> & <code>log~b~n = j</code> -> <code>b^j^ = n</code>

Multiply -> <code>b^i^ * b^j^ = m * n</code>

Simplify -> <code>b<sup>i+j</sup> = m * n</code>

Apply <code>log~b~</code> -> <code>i + j = log~b~m\*n</code>

Replace `i` and `j` -> <code>log~b~m + log~b~n = log~b~m\*n</code>

## Quotient law

<code>log~b~m/n = log~b~m - log~b~n</code>

**Proof:**

<code>log~b~m = i</code> -> <code>b^i^ = m</code> & <code>log~b~n = j</code> -> <code>b^j^ = n</code>

Divide -> <code>b^i^ / b^j^ = m / n</code>

Simplify -> <code>b<sup>i-j</sup> = m / n</code>

Apply <code>log~b~</code> -> <code>i - j = log~b~m/n</code>

Replace `i` and `j` -> <code>log~b~m - log~b~n = log~b~m/n</code>
