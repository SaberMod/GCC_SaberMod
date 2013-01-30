
#ifdef BIG_PAGE_SIZE
/* TODO - Replace '4096' below with correct big page size.  */
#define VTV_PAGE_SIZE 4096
#else 
#define VTV_PAGE_SIZE 4096
#endif

/* Page-aligned symbol to mark beginning of .vtable_map_vars section.  */
char _vtable_map_vars_start []
__attribute__ ((__visibility__ ("protected"), used, aligned(VTV_PAGE_SIZE),
                section(".vtable_map_vars")))
  = { };

