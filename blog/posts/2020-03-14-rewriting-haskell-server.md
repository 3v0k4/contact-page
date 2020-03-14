---
title: Rewriting to Haskell–Deployment
description: Everything needed to deploy and run Stream (Servant) on the server side
author: Riccardo
tags:
  - FunctionalProgramming
  - Haskell
  - Servant
---

This is part of a series:

- [Rewriting to Haskell–Intro](https://odone.io/posts/2020-02-26-rewriting-haskell-intro.html)
- [Rewriting to Haskell–Project Setup](https://odone.io/posts/2020-03-03-rewriting-haskell-setup.html)

---

## Deploy with Hapistrano

Stream was born as a Rails application. For that reason, we have been using [Capistrano](https://capistranorb.com/) to deploy it. That's why for the Servant code we have decided to employ [Hapistrano](https://hackage.haskell.org/package/hapistrano):

> Hapistrano makes it easy to reliably deploy Haskell applications to a server.
>
> Following popular libraries like Ruby's <http://capistranorb.com/ Capistrano>, Hapistrano does the work of building the application with dependencies into a distinct folder, and then atomically moves a symlink to the latest complete build.
>
> This allows for atomic switchovers to new application code after the build is complete. Rollback is even simpler, since Hapistrano can just point the current symlink to the previous release.

This is how we are currently using Hapistrano to deploy the code:

```bash
hap deploy
# or
HAPISTRANO_REVISION=origin/feature_branch hap deploy
```

What follows is our `hap.yaml`:

```yml
deploy_path: '/home/stream/application-hs'
host: stream@stream.example.com
ssh_args:
  - "-A" # SSH agent forwarding
repo: 'git@github.com:LunarLogic/stream.git'
revision: "_env:HAPISTRANO_REVISION:origin/master"
build_script:
  - cd haskell && stack setup
  - cd haskell && stack build
  - cd haskell && stack install --local-bin-path .
restart_command: sudo systemctl restart stream-hs
```

## Server

First of all, we need to have Stack installed. This is needed because, with the above configuration, Hapistrano will build the app on the server on each deploy.

```bash
sudo wget -qO- https://get.haskellstack.org | sh
```

Secondly, we decided that, for the time being, we will be serving the Servant code under `/servant`. Also, our Servant app will be running on port 8080. So let's have Nginx do the right thing:

```bash
location /servant {
  proxy_pass http://127.0.0.1:8080;
}
```

Thirdly, we want Systemd to manage the Servant process. What follows is the unit configuration we are using:

```bash
[Unit]
Description=Servant App
After=nginx.service
After=syslog.target
After=network.target

[Service]
Type=simple
Restart=always
ExecStart=/home/stream/application-hs/current/haskell/haskell-exe
#                      ^ `deploy_path` for Hapistrano.
#                                     ^ `current` is where Hapistrano keeps the latest deployed app.
#                                             ^ We keep the Servant code in the repo in the `haskell/` folder.
#                                                     ^ Name of the executable.
WorkingDirectory=/home/stream/application-hs/current/haskell
StandardOutput=syslog
StandardError=syslog
SyslogIdentifier=servant
User=stream

[Install]
WantedBy=multi-user.target
```

Notice that `haskell-exe` lives inside the `current` release (i.e. latest) because we configure Hapistrano to `stack install --local-bin-path .` .

Lastly, we need to allow the stream user to restart the application by adding them to `/etc/sudoers`:

```bash
stream ALL=(ALL) NOPASSWD: /bin/systemctl restart stream-hs
```

We automated all of the steps with Ansible.

We invoke it with:

```bash
- role: haskell
  haskell__app_name: stream
  haskell__username: stream
```

And here's the role:

```bash
- name: Install Stack
  shell: "sudo wget -qO- https://get.haskellstack.org | sh"
  args:
    creates: "/usr/local/bin/stack"

- name: Configure Nginx
  copy:
    src: "{{ item }}"
    dest: "/etc/nginx/snippets/{{ haskell__app_name }}/{{ item }}"
  with_items:
    - haskell.conf
  notify: reload nginx

- name: Create haskell service in Systemd
  template:
    src: haskell.service.j2
    dest: /etc/systemd/system/{{ haskell__app_name }}-hs.service
    mode: 0644

- name: Enable haskell service in Systemd
  systemd:
    name: "{{ haskell__app_name }}-hs"
    enabled: yes
    daemon_reload: yes
    state: started

- name: Allow user to restart the application
  lineinfile:
    dest: /etc/sudoers
    state: present
    line: "{{ haskell__username }} ALL=(ALL) NOPASSWD: /bin/systemctl restart {{ haskell__app_name }}-hs"
```
