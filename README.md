# sshd-lint

sshd-lint checks a sshd_config file for adherence to security best
practices. It can be

## Wat?

Weird things in ssh config file parsing

sshd has two checking modes, -t and -T. Neither one alerts if config
directives are repeated, regardless of whether the value being
assigned is the same or different.

If you have two different values for a config file value, the *first*
one takes effect (wat?). So the following allows cleartext passwords,
but putting the directives in the other order turns off cleartext
passwords. Again, there is no warning nor error from sshd abouth the
duplicate lines.

```
PasswordAuthentication yes
PasswordAuthentication no
```

## sshd-lint's Best Practices

There should be no duplicate lines present in the ssh config file. The
settings below should be present:

```
PermitEmptyPasswords no
PasswordAuthentication no
HostbasedAuthentication no
PermitRootLogin no
IgnoreRhosts yes
Protocol 2
StrictModes yes
UsePrivilegeSeparation yes
```

## Output


If sshd-lint has any suggestions they will be printed to standard
output. The return value of the program will be `1` if there are
suggestions.

The output of sshd-lint will be "No suggestions" if there are no
suggestions, and the return value will be `0`.

**The format of sshd-lint may change while the project is still young,
so it is suggested that scripts using sshd-lint depend on the return
value, but not necessarily the exact formatting of output until the
project matures.**

## Author

Justin Leitgeb

## Copyright

(C) 2015 Stack Builders Inc.
