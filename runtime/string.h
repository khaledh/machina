#include <stdint.h>

typedef struct mc_string {
  uint64_t ptr;     // address of bytes
  uint32_t len;     // byte length
  uint8_t  tag;     // 0=ascii, 1=utf8
  uint8_t  _pad[3]; // explicit padding to keep layout stable
} mc_string_t;
