#!/usr/bin/env dtrace -s
/* sudo ./flame.d -c ./target/release/bench | stackcollapse.pl | flamegraph.pl >out.svg */
profile-1ms
/pid == $target && arg1/
{
    @[ustack(100)] = count();
}
