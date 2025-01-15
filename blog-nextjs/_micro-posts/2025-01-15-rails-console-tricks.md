---
title: Rails console tricks
description:
author: Riccardo
tags:
  - Rails
---

```ruby
hash = { a: 1, b: "string", c: [1, 2, 3] }

y hash

---
:a: 1
:b: string
:c:
- 1
- 2
- 3
```

```
class User
  def initialize
    @ivar = 1
  end
  def imet
  end
  def self.cmet
  end
end

# ---

ls User

User.methods: cmet
#<Class:Object>#methods: yaml_tag
User#methods: imet

# ---

ls User.new

User#methods: imet
instance variables: @ivar
```

```
app.root_path
# or any other routes
```
