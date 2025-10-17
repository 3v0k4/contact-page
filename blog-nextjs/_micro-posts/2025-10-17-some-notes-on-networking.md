---
title: Some notes on Networking
description:
author: Riccardo
tags:
  - Networking
---

- `ipconfig getifaddr en0` -> `192.168.55.106`
- `bin/rails server -b 0.0.0.0 -p 3000` -> Visit `http://192.168.55.106:3000/`
- `lsof -i :3000` -> `localhost:hbci` vs `lsof -i :3000 -nP` -> `127.0.0.1:3000`
- `localhost:3000` tries both IPv6 (`[::1]:3000`) and IPv4 (`127.0.0.1:3000`), look at `/etc/hosts/`
