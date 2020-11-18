---
title: Make Your Tests Fail Randomly (and Profit)
description: Using random values in automated tests to improve readability and the likelihood of failures in the presence of bugs or wrong mental models.
published: true
author: Riccardo
cover_image: https://odone.io/images/mural.jpg
tags:
  - Essential Skills
---

Recently, I started experimenting with random values in automated tests. It improves the readability of my code and the likelihood of failures in the presence of bugs or wrong mental models.

## A Story From Last Week

I was tasked with a new feature in the middle of the worst code you can imagine. I identified a seam where I could introduce the logic, and I was glad to see the surroundings were covered with tests.

However, as soon as I opened the specs, my heart missed a beat. Some tests were as dry as beef jerky:

```rb
it "works for a business owner" do
  perform_test(@business.owner)
end
```

Other tests were soaked wet:

```rb
describe 'for a wholesaler' do
  it 'with an existing payment card' do
    build_fixtures(cart_owner: nil)

    wholesaler = create(
      :wholesaler_org,
      :with_payment_card,
      business: @business
    )

    @business_cart.parent = wholesaler.owner

    user_session = build_auth_session_for_user(wholesaler.owner)
    user_session.sign_in

    order_create_params = {
      wholesaler: true,
      business_cart_id: @business_cart.id,
      fulfillment_type: 'shipping',
      fulfillment_date: (Time.zone.now + 1.week).iso8601,
      order_customer_id: wholesaler.owner.id,
      order_customer_email: wholesaler.email,
      order_customer_name: wholesaler.name,
      order_customer_first_name: wholesaler.owner.first_name,
      order_customer_last_name: wholesaler.owner.last_name,
      order_customer_phone_number: wholesaler.phone_number,
      fulfillment_location_type: 'Address',
      fulfillment_location_id: wholesaler.owner.default_address.id,
      payment_card_attributes: {
        payment_type: 'credit_card',
        id: wholesaler.payment_cards.first.id,
      },
    }

    user_session.post resource_path(@business.id), params: order_create_params

    expect_model_response(Order.last)
    expect_json(
      parent_type: 'WholesalerOrg',
      parent_id: wholesaler.id,
      order_customer_email: wholesaler.email,
      order_customer_name: wholesaler.name,
      order_customer_phone_number: wholesaler.phone_number,
      fulfillment_type: order_create_params[:fulfillment_type],
      fulfillment_date: Order.last.fulfillment_date.iso8601(3),
      fulfillment_location_type: Address.to_s,
      fulfillment_location_id: Address.last.id,
      total_price: @business_product.business_product_templates.first.unit_price + 0,
      total_product_price: @business_product.business_product_templates.first.unit_price,
      total_tax_price: 0,
      payment_source_id: wholesaler.payment_cards.first.id
    )
    expect_json_sizes('order_items', 1)

    expect(@business_cart.reload.total_price).to eq 0
  end
end
```

Both completely unreadable: it was not clear to me what was under test or how they even worked. So I created a brand new file:

```rb
it 'with wholesale order it does not create tax records' do
  owner =
    User.create!(
      first_name: random_string,
      last_name: random_string,
      email: random_email,
      password: random_string,
      type: 'business'
    )
  business =
    Business.create!(
      owner: owner,
      name: random_string,
      phone_number: random_string
    )
  wholesaler =
    User.create!(
      first_name: random_string,
      last_name: random_string,
      email: random_email,
      password: random_string,
      type: 'wholesaler'
    )
  business_order = create_business_order!(business: business, recipient: wholesaler)

  CalculateTaxesAndUpdatePrice.new.call(business_order: business_order)

  expect(BusinessOrderTaxSummary.count).to eq(0)
  expect(BusinessOrderTaxLineItem.count).to eq(0)
end
```

I made **noise explicit by substituting hard-coded values with random ones**. The second step was to extract some builders and just leave pure signal in the spec itself:

```rb
it 'with wholesale order it does not create tax records' do
  business = create_business!
  wholesaler = create_user!(type: 'wholesaler')
  business_order = create_business_order!(business: business, recipient: wholesaler)

  CalculateTaxesAndUpdatePrice.new.call(business_order: business_order)

  expect(BusinessOrderTaxSummary.count).to eq(0)
  expect(BusinessOrderTaxLineItem.count).to eq(0)
end
```

A tad more readable, huh? It's clear that `type: 'wholesaler'` makes the difference and what the test is assessing.

I made sure the suite was green and pushed to CI. A couple of minutes later, I realized the build was red: the new test failed. I tried again on my computer, green.

I joked with myself that by introducing random values, I introduced random failures. Actually, that was exactly what happened, but it turned out great. Thanks to that, I discovered that *wholesale* orders are tax-exempt.

**I discovered a business rule.**

Sure, in this case, I could have written two tests: one for retail and one for wholesale. But there's a couple of problems: I couldn't test all combinations and I would write tests according to my bias. Random values defuse both of those issues.

I decided to introduce another element of randomness: instead of hardcoding one order-item per order in `create_business_order!`, I let the code generate an array of length 0, 1, or 2.

```rb
def create_business_order!(business:)
  BusinessOrder.create!(
    order_items: many { build_order_item },
    # ...
  )
end

def many
  Array.new([0, 1, 2].sample).map do
    yield
  end
end
```

Boom: another failure. Tax calculations assumed there would always be at least one order-item and started failing with 0.

**I uncovered a bug in the code.**

To summarize, random values allowed me to upgrade my mental model and find a bug. But this is no silver bullet, at least not in this form and shape.

Firstly, it's impossible to **reproduce deterministically** a failing test run: random values change every time.

This can be solved easily, at least in Ruby. The `rspec` test runner uses a seed value to randomly generate the order to run specs. The following line primes the Ruby's pseudo-random number generator with the same value:

```rb
srand(RSpec.configuration.seed)
```

With that in place, executing `rspec --seed 1234` twice will both run tests in the same order and generate the same "random" values.

Secondly, it would be best if a **test either always passed or not**. With random values, it's not possible to guarantee that level of determinism. But running the tests multiple times gets it close enough. Remember, even when the test is run only once, it still covers more than a test with hardcoded values; it just takes more time.

In `rspec` it could be achieved with:

```rb
100.times do
  it "tests something with random values" do
  end
end
```

*This is an excellent idea; you are a genius!*

I wish I could take credit for it. I stole this technique from Romeu Moura after attending his [Domain Invariants & Property-Based Testing for the Masses](https://www.youtube.com/watch?v=pX44CoRSIpg). And I'm guilty as charged since I pop up in the recording several times. I had short hair and wasn't as pink at the time, so bonus points if you can spot me.

Should you wish to explore this topic more in-depth, I suggest you look into Property-Based Testing. I also wrote [Property-based testing (with a sprinkle of JavaScript)](https://medium.com/hackernoon/property-based-testing-4330e3e77381) and [Diamond kata via property-based TDD in JavaScript](https://medium.com/hackernoon/diamond-kata-via-property-based-tdd-in-javascript-5fa99acd3e62). Both featured on HackerNoon, so you bet they are good reads!
