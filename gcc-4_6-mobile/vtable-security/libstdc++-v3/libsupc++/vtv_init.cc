/* Needs to build with C++ because the definition of __VLTChangePermission is in C++ */
#ifndef __cplusplus
#error "This file must be compiled with a C++ compiler"
#endif

/* Set the following macro to 1 to get internal debugging messages */
#define VTV_DEBUG 0


#include <stdio.h>
#endif

/* TODO: should we create a header for vtv_rts.c? */
extern void *
__VLTChangePermission (const char *arg1, int len);

void __VLTunprotect() __attribute__((constructor(98)));
void __VLTunprotect()
{
#if (VTV_DEBUG == 1)
  fprintf(stderr, "in __VLTunprotect\n");
#endif
  /* TODO: Change interface so you dont have to pass length */
  __VLTChangePermission("rw", 2);
}

void __VLTprotect() __attribute__((constructor(100)));
void __VLTprotect()
{
#if (VTV_DEBUG == 1)
  fprintf(stderr, "in __VLTprotect\n");
#endif
  /* TODO: Change interface so you dont have to pass length */
  __VLTChangePermission("ro", 2);
}
