#include <string.h>

void memcpy_off( void *dst, int doff, void *src, int soff, size_t len )
{
  memcpy( (char *)dst + doff, (char *)src + soff, len );
}

void memmove_off( void *dst, int doff, void *src, int soff, size_t len )
{
  memmove( (char *)dst + doff, (char *)src + soff, len );
}

void memset_off( void *dst, int doff, int c, size_t len )
{
  memset( (char *)dst + doff, c, len );
}

