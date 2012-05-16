#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#include "vtvmalloc.h"
#include <obstack.h>

/* TODO: Need to protect the following variables */
static struct obstack VTV_obstack;
static unsigned long page_size = 0;
static void * current_chunk = 0;
static size_t current_chunk_size = 0;

/* TODO: Define what is the expected behavior of assert and error */
/* Handling of runtime error */
static void VTV_error()
{
  abort();
}

#define VTV_assert(EXPR) ((void)(!(EXPR) ? VTV_error() : 0))

void VTV_protect()
{
  struct _obstack_chunk * ci;
  ci = current_chunk;
  while (ci)
  {
    VTV_assert(((unsigned long)ci & (page_size - 1)) == 0);
    if (mprotect(ci, (ci->limit - (char *)ci), PROT_READ) == -1)
      VTV_error();
    ci = ci->prev;
  }
}

void VTV_unprotect()
{
  struct _obstack_chunk * ci;
  ci = current_chunk;
  while (ci)
  {
    VTV_assert(((unsigned long)ci & (page_size - 1)) == 0);
    if (mprotect(ci, (ci->limit - (char *)ci), PROT_READ | PROT_WRITE) == -1)
      VTV_error();
    ci = ci->prev;
  }
}

/* Allocates a chunk of memory that is aligned to a page boundary.
   The amount of memory requested must be a multiple of the page size */
void * obstack_chunk_alloc(size_t size)
{
  /* TODO: Why do we need to support chunk sizes less that page size? */
  /* Get size to next multiple of page_size */
  size = (size + (page_size - 1)) & (~(page_size - 1));
  VTV_assert((size & (page_size - 1)) == 0);
  void * allocated;
  if (posix_memalign(&allocated, page_size, size) != 0)
    VTV_error();

  VTV_assert(allocated != NULL);
  current_chunk = allocated;
  current_chunk_size = size;
  return allocated;
}

void obstack_chunk_free(size_t size)
{
  /* Do nothing. For our purposes there should be very little de-allocation. */
}

void VTV_init()
{
  page_size = sysconf(_SC_PAGE_SIZE);

  obstack_chunk_size (&VTV_obstack) = page_size;
  obstack_alignment_mask(&VTV_obstack) = sizeof(long) - 1;
  /* We guarantee that the obstack alloc failed handler will never be called because
     in case the allocation of the chunk fails, it will never return */
  obstack_alloc_failed_handler = NULL;

  obstack_init(&VTV_obstack);
}

void * VTV_malloc(size_t size)
{
  return obstack_alloc (&VTV_obstack, size);
}

void VTV_free(void * ptr)
{
  /* Do nothing. We dont care about recovering unneded memory */
}
