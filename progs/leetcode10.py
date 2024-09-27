def match_char(c, p_c):
    return p_c == '.' or c == p_c

def match_pattern(s, p):
    if s == '' and p == '': return True
    if p == '': return False # pattern empty, but more string left
    if len(p) >= 2 and p[1] == '*':
        # right branch: skip star
        if match_pattern(s, p[2:]): return True
        # but if that fails, then we have to use the star
        return (
            len(s) # for that, we need at least one char in s
            and match_char(s[0], p[0]) # it must match the head of p
            and match_pattern(s[1:], p) # and p has to match the rest of s
        )
    else: # we're not handling a star, so we need:
        return (
            len(s) # there to be at least one character in s
            and match_char(s[0], p[0]) # that it match the head of p
            and match_pattern(s[1:], p[1:])
            # that the rest of the string match the rest of the pattern
        )

def match_pattern_dp(s, p, cache):
    if s == '' and p == '': return True
    if p == '': return False

    # answer already known:
    if (s, p) in cache: return cache[(s, p)]

    if len(p) >= 2 and p[1] == '*':
        outcome = (
            # skip star:
            match_pattern_dp(s, p[2:], cache)
            # or use star:
            or len(s)
            and match_char(s[0], p[0])
            and match_pattern_dp(s[1:], p, cache)
        )

    else:
        outcome = (
            len(s)
            and match_char(s[0], p[0])
            and match_pattern_dp(s[1:], p[1:], cache)
        )

    cache[(s, p)] = outcome
    return outcome

def preprocess(p):
    out = []
    i = 0
    while i < len(p):
        if i+1 < len(p) and p[i+1] == '*':
            out.append(p[i:i+2])
            i += 2
        else:
            out.append(p[i])
            i += 1
    return out

def match_pattern_tabular(s, p):
    p = preprocess(p)
    T = [[True] + [False] * len(s)]
    for i in range(1, len(p) + 1):
        p_c = p[-i]
        T[i][0] = p_c.endswith('*') and T[i-1][0]
        for j in range(1, len(s) + 1):
            c = s[-j]
            T[i][j] = (
                match_char(p_c, c) and T[i-1][j-1]
                if not p_c.endswith('*') else (
                    # use star
                    match_char(p_c[0], c) and T[i][j-1]
                    # skip star
                    or T[i-1][j]
                )
            )
    return T[len(p)][len(s)]

def epsilon_closure(nfa, i):
    closure = set([i]) # the state itself is part of the closure
    for (letter, j) in nfa[i]:
        if letter != '': continue
        closure.update(epsilon_closure(nfa, j))
    return closure

def run_nfa(nfa, s):
    active_states = epsilon_closure(nfa, 0)

    for c in s:
        # each iteration of the outermost loop
        # calculates a new set of active states.
        next_active_states = set()
        for i in active_states:
            for (letter, j) in nfa[i]:
                if match_char(c, letter):
                    next_active_states.update(epsilon_closure(nfa, j))
        active_states = next_active_states

    # accept or reject the string according to whether
    # the accepting state is among the active states, now that
    # the entire string has been traversed.
    return len(nfa) - 1 in active_states
