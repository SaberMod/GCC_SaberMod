/* TODO: add copyright and comments */

#ifndef _VTVMALLOC_H
#define _VTVMALLOC_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>

/* Alignment mask for any object returned by the VTV memory pool */
#define VTV_ALIGNMENT_MASK (0x7)

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

#endif /* vtvmalloc.h */
