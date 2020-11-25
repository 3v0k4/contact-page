---
title: How would I do it in Haskell?
description: Here's a couple of examples where I contaminated Ruby with functional intuitions.
published: true
author: Riccardo
cover_image: https://odone.io/images/rubies.jpg
tags:
  - Functional Programming
---

Since I started playing with functional programming, not only I've dot-chained the hell out of object-oriented code, but I often catch myself thinking: *how would I do this in Haskell?*

Here's a couple of examples that come to mind where I contaminated Ruby with functional intuitions.

## Nested Loops and Filtering

During the [Global Day Of Code Retreat](https://www.coderetreat.org), together with [Joanna](https://www.linkedin.com/in/jo-wojtowicz/), we were writing a method to find the eight neighbors of a cell. In other words, given `[ 1, 1 ]` as an input we wanted to generate the following output:

```rb
[ [ 0, 0 ], [ 0, 1 ], [ 0, 2 ]
  [ 1, 0 ],         , [ 1, 2 ]
  [ 2, 0 ], [ 2, 1 ], [ 2, 2 ]
]
```

We started with a simple implementation:

```rb
def neighbors(cell)
  [ -1, 0, 1 ].map do |y|
    [ -1, 0, 1 ].map do |x|
       [ cell.first + y, cell.last + x ]
    end
  end
end

# [1, 1]
# [[[0, 0], [0, 1], [0, 2]], [[1, 0], [1, 1], [1, 2]], [[2, 0], [2, 1], [2, 2]]]
```

To remove the double nesting in the output, we reached for `flatten`:

```rb
def neighbors(cell)
  [ -1, 0, 1 ].map do |y|
    [ -1, 0, 1 ].map do |x|
       [ cell.first + y, cell.last + x ]
    end
  end.flatten # <=
end

# [1, 1]
# [0, 0, 0, 1, 0, 2, 1, 0, 1, 1, 1, 2, 2, 0, 2, 1, 2, 2]
```

Oops, that was one `flatten` too much. We tried again with `flat_map`:

```rb
def neighbors(cell)
  [ -1, 0, 1 ].flat_map do |y| # <=
    [ -1, 0, 1 ].map do |x|
       [ cell.first + y, cell.last + x ]
    end
  end
end

# [1, 1]
# [[0, 0], [0, 1], [0, 2], [1, 0], [1, 1], [1, 2], [2, 0], [2, 1], [2, 2]]
```

Yay! The last piece was to remove the current cell from the output:

```rb
def neighbors(cell)
  [ -1, 0, 1 ].flat_map do |y|
    [ -1, 0, 1 ].map do |x|
       [ cell.first + y, cell.last + x ]
    end
  end.reject { |neighbor| neighbor == cell } # <=
end

# [1, 1]
# [[0, 0], [0, 1], [0, 2], [1, 0], [1, 2], [2, 0], [2, 1], [2, 2]]
```

There was something inelegant about that code, though. To hell with the exercise, we wasted the rest of the session refactoring the method.

The nested loop looked ugly. Not to count we were generating an *invalid* neighbor, only to filter it out later. Here's what we came up with:

```rb
def neighbors(cell)
  [ -1, 0, 1 ]
    .repeated_permutation(2)
    .reject { |permutation| permutation == [ 0, 0 ] }
    .map { |y, x| [ cell.first + y, cell.last + x ] }
end

# [1, 1]
# [[0, 0], [0, 1], [0, 2], [1, 0], [1, 2], [2, 0], [2, 1], [2, 2]]
```

## A Semigroup in the Wild

A few days ago, I stumbled upon the following code in a production codebase:

```rb
def group_by_product(items_grouped_by_template)
  items_grouped_by_template.select { |item| item.fetch(:variation_type) == 'quantity' }
                           .group_by { |item| item[:product_id] }
                           .each_with_object([]) do |(_product_id, order_items), array|
    array << item_grouped_by_product(order_items)
  end
end

def items_grouped_by_template
  @order_items_grouped_by_template.each_with_object([]) do |(template, order_items), array|
    array << item_grouped_by_template(template, order_items)
  end
end

def item_grouped_by_template(template, order_items)
   {
     id: template.id,
     variation_type: template.product_template.template_type,
     variation_name: template.product_template.name,
     product_id: template.product.id,
     product_name: template.product.name,
     total_quantity: product_template_total_quantity(template, order_items),
     reporting_category: reporting_category(template),
   }
end

def item_grouped_by_product(order_items)
   {
     id: order_items.first.fetch(:id),
     variation_type: order_items.first.fetch(:variation_type),
     product_id: order_items.first.fetch(:product_id),
     product_name: order_items.first.fetch(:product_name),
     total_quantity: product_total_quantity(order_items),
     reporting_category: order_items.first.fetch(:reporting_category),
   }
end
```

If you don't understand it, don't worry, I'm there with you. I speculate the author didn't understand it, either.

However, after massaging the code for a while, I noticed one thing: there's a hidden data structure that gets combined with itself: I found a [semigroup](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Semigroup.html)!

I created a `Summary` that can be `concat`ed with another one:

```rb
Summary = Struct.new(
  :id,
  :variation_type,
  :variation_name,
  :product_id,
  :product_name,
  :total_quantity,
  :category,
  keyword_init: true
) do
  def concat(other)
    Summary.new(
      id: id,
      variation_type: variation_type,
      variation_name: "#{variation_name} + #{other.variation_name}",
      product_id: product_id,
      product_name: product_name,
      total_quantity: [self, other].map(&:total_quantity).reduce(:+),
      category: category
    )
  end
end
```

With that in place, I could refactor to something similar to what follows:

```rb
product_product_templates
  .map { |product_product_template| to_summary(product_product_template) }
  .reduce { |acc, summary| acc.concat(summary) }

def to_summary(product_product_template)
  Summary.new(
    id: product_product_template.id,
    variation_type: product_product_template.product_template.template_type,
    variation_name: product_product_template.product_template.name,
    product_id: product_product_template.product.id,
    product_name: product_product_template.product.name,
    total_quantity: product_template_total_quantity(product_product_template),
    category: category(product_product_template)
  )
end
```

Still ugly, but at least I can reason about it.
