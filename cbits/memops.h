#ifndef haskell_primitive_memops_h
#define haskell_primitive_memops_h

#include <stdlib.h>

void memcpy_off( void *dst, int doff, void *src, int soff, size_t len );
void memmove_off( void *dst, int doff, void *src, int soff, size_t len );
void memset_off( void *dst, int doff, int c, size_t len );

#endif

