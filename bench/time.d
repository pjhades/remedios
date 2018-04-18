#!/usr/bin/env dtrace -s

#pragma D option quiet

pid$target::*Parser??parse[!_]*:entry
{
    self->ts = timestamp;
}

pid$target::*Compiler??compile[!_]*:return
{
    printf("%20s: %d ns\n", "parse & compile", timestamp - self->ts);
    self->ts = 0;
}

pid$target::*Vm??run*:entry
{
    self->ts = timestamp;
}

pid$target::*Vm??run*:return
{
    printf("%20s: %d ns\n", "run vm", timestamp - self->ts);
    self->ts = 0;
}
