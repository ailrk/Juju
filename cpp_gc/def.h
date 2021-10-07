#ifndef _IMP_ALLOCATOR
#define _IMP_ALLOCATOR
#define FREE_LIST 0
#define FIRST_FIT 1
#define NEXT_FIT 2
#define BEST_FIT 3
#endif

#ifndef _IMP_COLLECTOR
#define _IMP_COLLECTOR
#define MARK_SWEEP 0
#define MARK_COMPACT 1
#define REFERENCE_COUNT 2
#endif
