---
title: `form_with` form object
description:
author: Riccardo
tags:
  - Rails
---

The `form_with` helper accepts an `ActiveModel::Model`:

```ruby
<%= form_with model: TeamMemberForm.new do |f| %>
  <%= f.text_field :full_name %><br />
  <%= f.email_field :email %><br />
  <%= f.submit %>
<% end %>
```

You can create a form object like the following:

```ruby
class TeamMemberForm
  validates ...

  def self.model_name
    ActiveModel::Name.new(self, nil, 'TeamMember')
  end
end
```

By using `ActiveModel::Name`, Rails will take care of filling `name`s and `id`s as follows:

```
<input type="text" name="team_member[full_name]" id="team_member_full_name">
<input type="email" name="team_member[email]" id="team_member_email">
```

So in the controller you can:

```ruby
params.require(:team_member).permit(:full_name, :email)
```
