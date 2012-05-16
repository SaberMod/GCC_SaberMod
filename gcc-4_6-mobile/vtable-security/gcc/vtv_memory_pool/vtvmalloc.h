/* TODO: add copyright and comments */

#ifndef _VTVMALLOC_H
#define _VTVMALLOC_H 1

#include <stdlib.h>

/* Alignment mask for any object returned by the VTV memory pool */
#define VTV_ALIGNMENT_MASK (0x7)

void VTV_malloc_init();
/* TODO: Do we need an interface that destroys everything? */

void * VTV_malloc(size_t size);
void VTV_free(void * ptr);

#endif /* vtvmalloc.h */
