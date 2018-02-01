# Contact Page

Be sure to have
- `ansible/inventory` with the ip of the target container
- `ansible/secrets.yml` with the same structure as `ansible/secrets.yml.example`

And run
```
cd ansible
ansible-playbook -i inventory -u riccardo -s playbook.yml
```
on the first run use the user with the correct privileges.
