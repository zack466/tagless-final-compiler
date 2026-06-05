// C-side runtime for the scheme compiler. Provides the two extern helpers
// declared by *scheme-runtime* plus the C entry point that drives qbe_main.

#include <stdint.h>
#include <stdlib.h>

extern int qbe_main(void);

uint64_t _scheme_alloc(uint64_t bytes) {
    void *p = malloc((size_t)bytes);
    if (!p) abort();
    return (uint64_t)p;
}

// Declared :u64 in blub so the typechecker is happy. The C function never
// returns: blub will fall through to dead code on the QBE side after a call
// to abort(), but that's fine because abort() doesn't return.
uint64_t _scheme_panic(void) {
    abort();
}

int main(void) {
    return qbe_main();
}
