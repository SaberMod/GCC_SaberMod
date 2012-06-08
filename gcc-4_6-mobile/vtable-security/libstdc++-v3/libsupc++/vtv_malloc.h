/* TODO: add copyright and comments */

#ifndef _VTV_MALLOC_H
#define _VTV_MALLOC_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>

/* Alignment mask for any object returned by the VTV memory pool */
#ifdef __LP64__
#define VTV_ALIGNMENT_MASK (0x7)
#else
#define VTV_ALIGNMENT_MASK (0x3)
#endif

extern void VTV_malloc_init ();
/* TODO: Do we need an interface that destroys everything? */

extern void * VTV_malloc (size_t size);
extern void VTV_free (void * ptr);
extern void VTV_malloc_stats (void);

extern void VTV_protect (void);
extern void VTV_unprotect (void);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* vtv_malloc.h */
