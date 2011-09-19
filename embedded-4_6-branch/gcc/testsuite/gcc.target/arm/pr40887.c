/* { dg-options "-O2 -march=armv5te" }  */
/* { dg-final { scan-assembler "blx" } } */
/* { dg-prune-output "switch .* conflicts with" } */

int (*indirect_func)();

int indirect_call()
{
    return indirect_func();
}
