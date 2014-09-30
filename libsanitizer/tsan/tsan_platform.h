//===-- tsan_platform.h -----------------------------------------*- C++ -*-===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of ThreadSanitizer (TSan), a race detector.
//
// Platform-specific code.
//===----------------------------------------------------------------------===//

/*
C++ linux memory layout:
0000 0000 0000 - 03c0 0000 0000: protected
03c0 0000 0000 - 1000 0000 0000: shadow
1000 0000 0000 - 3000 0000 0000: protected
3000 0000 0000 - 4000 0000 0000: metainfo (memory blocks and sync objects)
4000 0000 0000 - 6000 0000 0000: protected
6000 0000 0000 - 6200 0000 0000: traces
6200 0000 0000 - 7d00 0000 0000: -
7d00 0000 0000 - 7e00 0000 0000: heap
7e00 0000 0000 - 7fff ffff ffff: modules and main thread stack

Go linux and darwin memory layout:
0000 0000 0000 - 0000 1000 0000: executable
0000 1000 0000 - 00f8 0000 0000: -
00c0 0000 0000 - 00e0 0000 0000: heap
00e0 0000 0000 - 1000 0000 0000: -
1000 0000 0000 - 1380 0000 0000: shadow
1460 0000 0000 - 2000 0000 0000: -
3000 0000 0000 - 4000 0000 0000: metainfo (memory blocks and sync objects)
4000 0000 0000 - 6000 0000 0000: -
6000 0000 0000 - 6200 0000 0000: traces
6200 0000 0000 - 7fff ffff ffff: -

Go windows memory layout:
0000 0000 0000 - 0000 1000 0000: executable
0000 1000 0000 - 00f8 0000 0000: -
00c0 0000 0000 - 00e0 0000 0000: heap
00e0 0000 0000 - 0100 0000 0000: -
0100 0000 0000 - 0560 0000 0000: shadow
0560 0000 0000 - 0760 0000 0000: traces
0760 0000 0000 - 07d0 0000 0000: metainfo (memory blocks and sync objects)
07d0 0000 0000 - 07ff ffff ffff: -
*/

#ifndef TSAN_PLATFORM_H
#define TSAN_PLATFORM_H

#include "tsan_defs.h"
#include "tsan_trace.h"

#if defined(__LP64__) || defined(_WIN64)
namespace __tsan {

#if defined(TSAN_GO)
static const uptr kLinuxAppMemBeg = 0x000000000000ULL;
static const uptr kLinuxAppMemEnd = 0x04dfffffffffULL;
# if SANITIZER_WINDOWS
static const uptr kLinuxShadowMsk = 0x010000000000ULL;
static const uptr kMetaShadow     = 0x076000000000ULL;
static const uptr kMetaSize       = 0x007000000000ULL;
# else  // if SANITIZER_WINDOWS
static const uptr kLinuxShadowMsk = 0x200000000000ULL;
static const uptr kMetaShadow     = 0x300000000000ULL;
static const uptr kMetaSize       = 0x100000000000ULL;
# endif  // if SANITIZER_WINDOWS
#else  // defined(TSAN_GO)
static const uptr kMetaShadow     = 0x300000000000ULL;
static const uptr kMetaSize       = 0x100000000000ULL;
static const uptr kLinuxAppMemBeg = 0x7cf000000000ULL;
static const uptr kLinuxAppMemEnd = 0x7fffffffffffULL;
#endif

static const uptr kLinuxAppMemMsk = 0x7c0000000000ULL;

#if SANITIZER_WINDOWS
const uptr kTraceMemBegin = 0x056000000000ULL;
#else
const uptr kTraceMemBegin = 0x600000000000ULL;
#endif
const uptr kTraceMemSize = 0x020000000000ULL;

// This has to be a macro to allow constant initialization of constants below.
#ifndef TSAN_GO
#define MemToShadow(addr) \
    ((((uptr)addr) & ~(kLinuxAppMemMsk | (kShadowCell - 1))) * kShadowCnt)
#define MemToMeta(addr) \
    (u32*)(((((uptr)addr) & ~(kLinuxAppMemMsk | (kMetaShadowCell - 1))) \
    / kMetaShadowCell * kMetaShadowSize) | kMetaShadow)
#else
#define MemToShadow(addr) \
    (((((uptr)addr) & ~(kShadowCell - 1)) * kShadowCnt) | kLinuxShadowMsk)
#define MemToMeta(addr) \
    (u32*)(((((uptr)addr) & ~(kMetaShadowCell - 1)) \
    / kMetaShadowCell * kMetaShadowSize) | kMetaShadow)
#endif

static const uptr kLinuxShadowBeg = MemToShadow(kLinuxAppMemBeg);
static const uptr kLinuxShadowEnd =
    MemToShadow(kLinuxAppMemEnd) | 0xff;

static inline bool IsAppMem(uptr mem) {
#if defined(TSAN_GO)
  return mem <= kLinuxAppMemEnd;
#else
  return mem >= kLinuxAppMemBeg && mem <= kLinuxAppMemEnd;
#endif
}

static inline bool IsShadowMem(uptr mem) {
  return mem >= kLinuxShadowBeg && mem <= kLinuxShadowEnd;
}

static inline uptr ShadowToMem(uptr shadow) {
  CHECK(IsShadowMem(shadow));
#ifdef TSAN_GO
  return (shadow & ~kLinuxShadowMsk) / kShadowCnt;
#else
  return (shadow / kShadowCnt) | kLinuxAppMemMsk;
#endif
}

void FlushShadowMemory();
void WriteMemoryProfile(char *buf, uptr buf_size, uptr nthread, uptr nlive);
uptr GetRSS();

void InitializePlatform();
void FinalizePlatform();

// The additional page is to catch shadow stack overflow as paging fault.
// Windows wants 64K alignment for mmaps.
const uptr kTotalTraceSize = (kTraceSize * sizeof(Event) + sizeof(Trace)
    + (64 << 10) + (64 << 10) - 1) & ~((64 << 10) - 1);

uptr ALWAYS_INLINE GetThreadTrace(int tid) {
  uptr p = kTraceMemBegin + (uptr)tid * kTotalTraceSize;
  DCHECK_LT(p, kTraceMemBegin + kTraceMemSize);
  return p;
}

uptr ALWAYS_INLINE GetThreadTraceHeader(int tid) {
  uptr p = kTraceMemBegin + (uptr)tid * kTotalTraceSize
      + kTraceSize * sizeof(Event);
  DCHECK_LT(p, kTraceMemBegin + kTraceMemSize);
  return p;
}

void *internal_start_thread(void(*func)(void*), void *arg);
void internal_join_thread(void *th);

// Says whether the addr relates to a global var.
// Guesses with high probability, may yield both false positives and negatives.
bool IsGlobalVar(uptr addr);
int ExtractResolvFDs(void *state, int *fds, int nfd);
int ExtractRecvmsgFDs(void *msg, int *fds, int nfd);

int call_pthread_cancel_with_cleanup(int(*fn)(void *c, void *m,
    void *abstime), void *c, void *m, void *abstime,
    void(*cleanup)(void *arg), void *arg);

}  // namespace __tsan

#else  // defined(__LP64__) || defined(_WIN64)
# error "Only 64-bit is supported"
#endif

#endif  // TSAN_PLATFORM_H
