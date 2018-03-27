#!/usr/bin/env dtrace -s

#pragma D option quiet

pid$target::*re_match*:entry
{
    self->ts_match_start = timestamp;
}

pid$target::*re_match*:return
{
    printf("%-60s: %d ns\n", probefunc, timestamp - self->ts_match_start);
    self->ts_match_start = 0;
}

pid$target::*Parser??parse[!_]*:entry
{
    self->ts_parse_start = timestamp;
}

pid$target::*Parser??parse[!_]*:return
{
    printf("%-60s: %d ns\n", probefunc, timestamp - self->ts_parse_start);
    self->ts_parse_start = 0;
}

pid$target::*Compiler??compile[!_]*:entry
{
    self->ts_compile_start = timestamp;
}

pid$target::*Compiler??compile[!_]*:return
{
    printf("%-60s: %d ns\n", probefunc, timestamp - self->ts_compile_start);
    self->ts_compile_start = 0;
}

pid$target::*Vm??run*:entry
{
    self->ts_vm_start = timestamp;
}

pid$target::*Vm??run*:return
{
    printf("%-60s: %d ns\n", probefunc, timestamp - self->ts_vm_start);
    self->ts_vm_start = 0;
}
