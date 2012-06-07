#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdio.h>

#include "vtvmalloc.h"
#include <obstack.h>

/* Set the following macro to 1 to get internal debugging messages */
#define VTV_DEBUG 0

/* TODO: Need to protect the following variables */
static struct obstack VTV_obstack;
static unsigned long page_size = 0;
static void * current_chunk = 0;
static size_t current_chunk_size = 0;

/* TODO: Define what is the expected behavior of assert and error */
/* Handling of runtime error */
static void
VTV_error (void)
{
  abort();
}

#define VTV_assert(EXPR) ((void)(!(EXPR) ? VTV_error() : (void) 0))

void
VTV_protect (void)
{
  struct _obstack_chunk * ci;
  ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
  {
    VTV_assert(((unsigned long)ci & (page_size - 1)) == 0);
    if (mprotect(ci, (ci->limit - (char *)ci), PROT_READ) == -1)
      VTV_error();
    ci = ci->prev;
  }
#if (VTV_DEBUG == 1)
  {
    int count = 0;
    ci = (struct _obstack_chunk *) current_chunk;
    while (ci)
    {
      count++;
      ci = ci->prev;
    }
    fprintf(stderr, "VTV_protect(): protected %d pages\n", count);
  }
#endif
}

void
VTV_unprotect (void)
{
  struct _obstack_chunk * ci;
  ci = (struct _obstack_chunk *) current_chunk;
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
static void *
obstack_chunk_alloc (size_t size)
{
  /* TODO: Why do we need to support chunk sizes less that page size? */
  /* Get size to next multiple of page_size */
  size = (size + (page_size - 1)) & (~(page_size - 1));
  VTV_assert((size & (page_size - 1)) == 0);
  void * allocated;
  if ((allocated = mmap (NULL, size, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS,  -1, 0)) == 0)
    VTV_error();

  VTV_assert(((unsigned long) allocated & (page_size - 1)) == 0);
  current_chunk = allocated;
  current_chunk_size = size;
  return allocated;
}

static void
obstack_chunk_free (size_t size)
{
  /* Do nothing. For our purposes there should be very little de-allocation. */
}

void
VTV_malloc_init (void)
{
  static int initialized = 0;

  /* Make sure we only execute the main body of this function ONCE.  */
  if (initialized)
    return;

  page_size = sysconf(_SC_PAGE_SIZE);

  obstack_chunk_size (&VTV_obstack) = page_size;
  obstack_alignment_mask(&VTV_obstack) = sizeof(long) - 1;
  /* We guarantee that the obstack alloc failed handler will never be called because
     in case the allocation of the chunk fails, it will never return */
  obstack_alloc_failed_handler = NULL;

  obstack_init(&VTV_obstack);
  initialized = 1;
}

void *
VTV_malloc (size_t size)
{
  return obstack_alloc (&VTV_obstack, size);
}

void
VTV_free (void * ptr)
{
  /* Do nothing. We dont care about recovering unneded memory */
}

void
VTV_malloc_stats (void)
{
  int count = 0;
   struct _obstack_chunk * ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
  {
    count++;
    ci = ci->prev;
  }
  fprintf(stderr, "VTV_malloc_stats:\n  Page Size = %d bytes\n  Number of pages = %d\n", page_size, count);
}
