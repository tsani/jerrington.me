#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define GENERATOR_STACK_LIMIT 1024

/**
 * Maximum number of variables is 64.
 */
typedef uint64_t truth_assignment;
typedef uint8_t var_index;

truth_assignment set_true(truth_assignment a, var_index i) {
    return a | (1UL << i);
}

truth_assignment set_false(truth_assignment a, var_index i) {
    return a & ~(1UL << i);
}

truth_assignment const EMPTY_TRUTH_ASSIGNMENT = 0;

struct frame {
    var_index i;
    truth_assignment a;
};

struct frame make_frame(var_index i, truth_assignment a) {
    return (struct frame) {
        .i = i,
        .a = a,
    };
}

#define MAX_VARS 64
#define STACK_LIMIT MAX_VARS

/**
 * This set of states comes from the three constructors of `stack` in the OCaml program.
 * We don't need to keep a separate FINISHED state because this state arises when we're in the
 * CONTINUE state but there are no more stack frames to pop.
 */
enum state {
    START = 1,
    CONTINUE = 0,
};

struct generator {
    // points to the first unused frame; invariant: frame_pointer < stack_capacity
    // seen differently, this is the count of frames in use.
    // Since we're not going to need 4 billion frames, we can use the most-significant-bit of the
    // frame pointer to store the generator state: this bit will be high when the generator is in
    // initial state, and low otherwise. Then, we can detect whether the generator is running or
    // finished according to the integer value of `frame_pointer`: when it's zero, there are no
    // more frames left and the generator is finished.
    uint64_t frame_pointer;

    // in the initial state, we need to know the number of variables, but otherwise
    // we just need the stack of frames.
    union {
        // The total number of variables to enumerate the truth assignments for.
        // At most MAX_VARS;
        var_index num_variables;

        // the call stack of the generator
        struct frame stack[STACK_LIMIT];
    } data;
};

enum state generator_state(struct generator *gen) {
    return gen->frame_pointer >> 63;
}

/**
 * Returns -1 if pushing fails: wrong generator state or stack is full.
 * Otherwise copies the given frame into the top of the stack, increments the frame pointer,
 * and returns 1. */
int gen_stack_push(struct generator * gen, struct frame const * frame) {
    if (gen->frame_pointer >= STACK_LIMIT) {
        return -1;
    }
    gen->data.stack[gen->frame_pointer++] = *frame;
    return 1;
}

/**
 * Returns -1 if popping is forbidden: wrong generator state.
 * Return 0 if the stack is empty.
 * Otherwise decrements the frame pointer, and copies the top frame into `out`,
 * and returns 1 */
int gen_stack_pop(struct generator * gen, struct frame * out) {
    if (START == generator_state(gen)) return -1;
    if (0 == gen->frame_pointer) return 0;
    *out = gen->data.stack[-- gen->frame_pointer];
    return 1;
}

/**
 * In the OCaml program, `enumerate_assignments` returns a function that calls `apply` on the
 * current state. Instead, the C program will directly manipulate the state, so
 * `enumerate_assignments` here simply sets up the initial state and returns it.
 */
struct generator enumerate_assignments(int num_variables) {
    return (struct generator) {
        .data.num_variables = num_variables,
        .frame_pointer = (uint64_t)1 << 63,
    };
}

/* For reference, `go` and `next` below are is translated from the OCaml program:

  let rec go n a s =
    if n = 0 then
      (state := s; Some a)
    else
     go (n-1) (true :: a) (Continue ({n; a}, s))
  and apply s = match s with
    | Start -> go n [] Finished (* Finished comes from fun () -> None *)
    | Continue ({n; a}, s) -> go (n-1) (false :: a) s
    | Finished -> None
  in

  The `option` type in the OCaml program is represented in C by the pair of the returned int value
  together with the out parameter `truth_assignment * ta`.
  The returned int functions as the tag of the option, indicating whether it's `None` or `Some`.
  Notice that `go` always returns `1` (Some) unless an error occurs.
 */

/**
 * Since go was a tail-recursive program in OCaml, we translate it to a while loop instead of to a
 * recursive C function.
 * The OCaml program `go` takes the stack as a parameter and pushes to it on each recursive call,
 * only saving the stack into the state right before returning.
 * Instead, the C program `go` will push stack frames directly into the state.
 * The next truth assignment is written directly into `ta`.
 * Returns 1 on success and -1 in case the generator encounters an error.
 */
int go(struct generator * gen, var_index i, truth_assignment * ta) {
    // due to the --> 'operator', `i` will have its value decremented by one inside the loop
    while (i --> 0) {
        *ta = set_true(*ta, i);
        // in the OCaml program, the frame that gets pushed by the recursive call
        // `go (n-1) (true :: a) (Continue ({n; a}, stk))`
        // stores the value `n`, but here we are storing n-1 as a consequence
        // of the decrement that happens in the while loop condition.
        struct frame frame = make_frame(i, *ta);
        if(-1 == gen_stack_push(gen, &frame)) return -1;
    }
    return 1;
}

/**
 * Pumps the generator for one more item.
 * This is what I translated `apply` into.
 * Returns 0 if the sequence ends, in which case `ta` is untouched.
 * Otherwise returns 1 and writes the next truth assignment into `ta`.
 * Return -1 in case the generator encounters an error.
 */
int next(struct generator * gen, truth_assignment * ta) {
    int status;
    struct frame frame;

    switch (generator_state(gen)) {
    case CONTINUE:
        status = gen_stack_pop(gen, &frame);

        if (0 == status) return 0;
        if (-1 == status) return -1;

        *ta = set_false(frame.a, frame.i);
        if (-1 == go(gen, frame.i, ta)) return -1;
        return 1;

    case START:
        *ta = EMPTY_TRUTH_ASSIGNMENT;
        gen->frame_pointer = 0;
        if(-1 == go(gen, gen->data.num_variables, ta)) return -1;
        return 1;
    }
}

int main() {
    struct generator gen = enumerate_assignments(5);
    int status = 0;
    truth_assignment a;

    for (;;) {
        status = next(&gen, &a);
        if (1 != status) break;
        printf("truth assignment: %d\n", a);
    }

    if (-1 == status) {
        printf("Generator encountered an error, sorry.\n");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
