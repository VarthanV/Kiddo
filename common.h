
#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define NAN_TAGGING

#define DEBUG_PRINT_CODE

#define DEBUG_TRACE_EXECUTION

#undef DEBUG_PRINT_CODE
#undef DEBUG_TRACE_EXECUTION

#define DEBUG_STRESS_GC
#define LINEBUFSIZE 1024 * sizeof(char)
#define UINT8_COUNT (UINT8_MAX + 1)

#endif