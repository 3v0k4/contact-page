---
title: Dialog with Rails & Hotwire
description:
author: Riccardo
tags:
  - Rails
---

![Clicking on Invite User opens a dialog that contains a form and an X button in the top-right corner to dismiss it](https://github.com/user-attachments/assets/abb4de5d-5cca-4587-b8d1-00cadd6366e2)

At the end of `<body>`:

```erb
<dialog data-controller="modal-dialog" class="tw-w-[calc(100%-0.5rem)] tw-max-w-xl tw-rounded-lg">
  <div class="tw-p-8">
    <button
      data-action="click->modal-dialog#close"
      class="tw-absolute tw-top-6 tw-right-8 tw-text-5xl tw-opacity-40 hover:tw-opacity-90 tw-select-none"
    >&times;</button>

    <%= turbo_frame_tag 'modal_dialog', data: { action: 'turbo:frame-load->modal-dialog#open' } %>
  </div>
</dialog>
```

Stimulus controller:

```js
import { Controller } from "@hotwired/stimulus";

export default class extends Controller {
  connect() {
    this.element.addEventListener("close", () => {
      this.element.classList.remove('open')
    });
  }

  disconnect() {
    this.close();
  }

  open() {
    this.element.showModal();
    this.element.classList.add('open')
  }

  close() {
    this.element.close();
    this.element.classList.remove('open')
  }
}
```

CSS:

```scss
// This scrolls the page to the top, which is a problem if the
// user opens the dialog not at the top of the page.
//
// The following would work but causes an horizontal layout
// shift when the scrollbar disappears:
//   .no-scroll {
//     overflow: hidden;
//   }
//
// Keep an eye on the following thread for better solutions:
// https://github.com/whatwg/html/issues/7732
body:has(dialog[open]) {
  overflow-y: scroll;
  position: fixed;
  width: 100%;
}

dialog {
  opacity: 0;
  transition: opacity 0.3s ease-out;
  box-shadow: 0 0 0 100vmax rgba(0, 0, 0, 0.8);
}

dialog.open {
  opacity: 1;
}

dialog::backdrop {
  display: none;
}
```

Button that opens the dialog:

```erb
<%= link_to(
  fa_icon('plus', text: 'Invite user'),
  new_dashboard_organization_team_member_path(@organization),
  class: '!tw-py-2 !tw-px-3 !tw-text-base btn btn-default',
  data: { turbo: true, turbo_frame: 'modal_dialog' }
) %>
```

Form that gets rendered into the dialog:

```erb
<div class="container tw-py-20">
  <div class="tw-max-w-xl">
    <p class="text-muted">
      <span data-toggle="tooltip" title="Organization name"><%= @organization.name %></span> / <%= link_to 'Team members', dashboard_organization_team_members_path(@organization) %>
    </p>

    <%= turbo_frame_tag 'modal_dialog' do %>
      <h1 class="tw-m-0 tw-mb-6">Invite team member</h1>

      <%= render partial: 'form', locals: {
        team_member_form: @team_member_form,
        form_method: :post,
          action_url: dashboard_organization_team_members_path,
          submit_value: 'Invite'
        } %>
    <% end %>
  </div>
</div>
```

Controller:

```ruby
def create
  team_member_form = TeamMemberForm::NewForm.new(new_team_member_params.merge(organization: organization, inviter_email: current_user.email))

  if team_member_form.save
    flash[:success] = 'The user has been added to the team members.'

    respond_to do |format|
      format.turbo_stream do
        render turbo_stream: turbo_stream.action(:redirect, dashboard_organization_team_members_path(organization))
      end
      format.html do
        redirect_to dashboard_organization_team_members_path(organization)
      end
    end
  else
    @team_member_form = team_member_form
    render :new
  end
end
```

Redirect:

```js
// Workaround for redirecting from within a Turbo Frame:
// https://github.com/hotwired/turbo-rails/pull/367
Turbo.StreamActions.redirect = function () {
  Turbo.visit(this.target);
};
```
