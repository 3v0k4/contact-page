---
title: Comment your regular expressions
author: Riccardo
tags:
  - RegExp
---

```ruby
# ❌
usa_postal_code_pattern = /\A\d{5}(-\d{4})?\z/

# ✅
usa_postal_code_pattern = %r{
  \A # Beginning of string
  \d{5} # 5 digits
  ( # ZIP+4
    - # Hyphen
    \d{4} # 4 digits
  )? # ZIP+4 is optional
  \z # End of string
}x
```

Source: [Comment your regular expressions](https://thoughtbot.com/blog/comment-your-regular-expressions)
