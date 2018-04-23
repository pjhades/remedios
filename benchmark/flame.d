#!/usr/bin/env dtrace -s

profile-100
/pid == $target && arg1/
{
    @[ustack(100)] = count();
}
