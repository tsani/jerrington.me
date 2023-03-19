import sys

def print_assignment(a):
    for x in a:
        sys.stdout.write('1' if x else '0')
    sys.stdout.write('\n')

def enumerate_assignments(n):
    a = [True] * 5
    def go(i):
        if i == n:
            yield a
            return
        a[i] = True
        yield from go(i + 1)
        a[i] = False
        yield from go(i + 1)
    yield from go(0)

def test():
    for x in enumerate_assignments(5):
        print_assignment(x)
