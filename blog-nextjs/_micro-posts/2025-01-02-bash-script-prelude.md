---
title: Bash script prelude
description:
author: Riccardo
tags:
  - Bash
---

```bash
#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
set -vx
```

Explanation:

**`#!/usr/bin/env bash`: interpret the subsequent lines with `bash`**

---

**`set -e`: exit immediately with error**

Script:

```bash
echo 1
not-existing-command
echo 2
```

Out:

```
1
not-existing-command: command not found
2
```

Script:

```bash
set -e

echo 1
not-existing-command
echo 2
```

Out:

```
1
not-existing-command: command not found
```

---

**`set -u`: exit immediately with unbound variable**

Script:

```bash
echo 1
echo $NOT_EXISTING_VARIABLE
echo 2
```

Out:

```
1

2
```

Script:

```bash
set -u

echo 1
echo $NOT_EXISTING_VARIABLE
echo 2
```

Out:

```
1
NOT_EXISTING_VARIABLE: unbound variable
```

---

**`set -o pipefail`: make errors fall through pipelines**

Script:

```bash
not-existing-command | sort
echo $?
```

Out:

```
not-existing-command: command not found
0
```

Script:

```bash
set -o pipefail

not-existing-command | sort
echo $?
```

Out:

```
not-existing-command: command not found
127
```

---

**`IFS=$'\n\t'`: Set input field separator (default: $' \n\t')**

Script:

```bash
string="1 2 3"

for i in $string; do
  echo "$i"
done

IFS=$'\n\t'
for i in $string; do
  echo "$i"
done
```

Out:

```
1
2
3
1 2 3
```

Script:

```bash
array=(
"a b"
"c d"
)

for x in ${array[@]}; do
  echo "$x"
done

IFS=$'\n\t'
for x in ${array[@]}; do
  echo "$x"
done
```

Out:

```
a
b
c
d
a b
c d
```

---

**`set -x`: print lines as they are executed**

Script:

```bash
set -x

echo 0
echo 1
```

Out:

```
+ echo 0
0
+ echo 1
1
```
