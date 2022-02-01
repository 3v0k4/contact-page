#!/usr/bin/env bash
set -euxo pipefail

cd ansible
ansible-playbook -i inventory -u riccardo playbook.yml
cd -
