[![Build Status](https://travis-ci.org/stackbuilders/sshd-lint.svg)](https://travis-ci.org/stackbuilders/sshd-lint)

# sshd-lint

sshd-lint checks a sshd_config file for adherence to security best
practices.

## Wat? Weird things happen in ssh config file parsing.

Like [in JavaScript](https://www.destroyallsoftware.com/talks/wat),
trying to understand how sshd reads your configuration file leads to
several "Wat?" moments.

### Wat #1 - Earliest option wins for some settings

sshd has two checking modes, -t and -T. Neither one alerts if config
directives are repeated, regardless of whether the value being
assigned is the same or different.

If you have two different values for some config file value, the
*first* one takes effect (wat?). So the following allows cleartext
passwords, but putting the directives in the other order turns off
cleartext passwords. Again, there is no warning nor error from sshd
abouth the duplicate lines.

```
PasswordAuthentication yes
PasswordAuthentication no
```

sshd-lint takes the weird sshd parsing into account and makes sure
that settings adhere to best practices. It also lets you know if there
are duplicated lines which may lead to unexpected behavior from the
SSH daemon.

### Wat #2 - No warning for contradictory settings

sshd allows contradictory settings. Fortunately, for certain options
like AllowUser, AllowGroup, DenyUser and DenyGroup, the most
restrictive option takes effect. Still, it's a bit unexpected that if
you add user 'joe' to both AllowUser and DenyUser, joe is Denied
without any warning that you told sshd to use contradictory settings.

sshd-lint considers contradictory settings to be an error.

## sshd-lint's Best Practices

There should be no duplicate lines present in the ssh config file
(with the exception of the HostKey configuration option, which we
ignore). Currently the following settings are considered best
practices by `sshd-lint`.

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

## Usage

Invoking `sshd-lint` without any arguments defaults to checking
`/etc/ssh/sshd_config`, which should be present on most systems
running OpenSSH. You can run `sshd_lint` on any configuration file
by specifying it after the `-f` argument, eg:

```
sshd-lint -f /tmp/new_sshd_config
```

`sshd_lint` supports a Nagios output mode, which can be enabled with
the `-n` flag.

Other options can be seen by invoking `sshd_lint` with the `-h` flag.

## Other Reading

Many of the sshd best practices come from the article, [Top 20 OpenSSH
Server Best Security
Practices](http://www.cyberciti.biz/tips/linux-unix-bsd-openssh-server-best-practices.html).


## Author

Justin Leitgeb

## License

MIT

## Copyright

(C) 2015 Stack Builders Inc.
