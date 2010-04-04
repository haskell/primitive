#include <string.h>

void memcpy_off( void *dst, int doff, void *src, int soff, size_t len )
{
  memcpy( (char *)dst + doff, (char *)src + soff, len );
}

