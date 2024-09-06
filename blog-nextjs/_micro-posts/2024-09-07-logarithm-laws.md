---
title: Logarithm laws
author: Riccardo
description: A quick recap of some fundamental properties of logarithms
tags:
  - Math
---

## Power law

<code>log<sub>b</sub>p<sup>n</sup> = n * log<sub>b</sub>p</code>

**Proof:**

<code>log<sub>b</sub>p = a</code> -> <code>b<sup>a</sup> = p</code>

Raise to `k` -> <code>b<sup>k\*a</sup> = p<sup>k</sup></code>

Apply <code>log<sub>b</sub></code> -> <code>k * a = log<sub>b</sub>p<sup>k</sup></code>

Replace `a` with <code>log<sub>b</sub>p</code> -> <code>k * log<sub>b</sub>p = log<sub>b</sub>p<sup>k</sup></code>

## Product law

<code>log<sub>b</sub>m\*n = log<sub>b</sub>m + log<sub>b</sub>n</code>

**Proof:**

<code>log<sub>b</sub>m = i</code> -> <code>b<sup>i</sup> = m</code> & <code>log<sub>b</sub>n = j</code> -> <code>b<sup>j</sup> = n</code>

Multiply -> <code>b<sup>i</sup> * b<sup>j</sup> = m * n</code>

Simplify -> <code>b<sup>i+j</sup> = m * n</code>

Apply <code>log<sub>b</sub></code> -> <code>i + j = log<sub>b</sub>m\*n</code>

Replace `i` and `j` -> <code>log<sub>b</sub>m + log<sub>b</sub>n = log<sub>b</sub>m\*n</code>

## Quotient law

<code>log<sub>b</sub>m/n = log<sub>b</sub>m - log<sub>b</sub>n</code>

**Proof:**

<code>log<sub>b</sub>m = i</code> -> <code>b<sup>i</sup> = m</code> & <code>log<sub>b</sub>n = j</code> -> <code>b<sup>j</sup> = n</code>

Divide -> <code>b<sup>i</sup> / b<sup>j</sup> = m / n</code>

Simplify -> <code>b<sup>i-j</sup> = m / n</code>

Apply <code>log<sub>b</sub></code> -> <code>i - j = log<sub>b</sub>m/n</code>

Replace `i` and `j` -> <code>log<sub>b</sub>m - log<sub>b</sub>n = log<sub>b</sub>m/n</code>
