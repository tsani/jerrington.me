const countdown = (recurse, n) => {
    if (n == 0) {
        console.log("No more bottles of beer on the wall.");
        return;
    }
    console.log(`${n} bottles of beer on the wall, ${n} bottles of beer!`);

    recurse(n-1);
}

const recursively = (f) => (...args) => f(recursively(f), ...args);

const delayedRecursively = (f, delay) =>
      (...args) => f((...args) =>
          setTimeout(() =>
              f(delayedRecursively(f, delay), ...args), delay
          ),
          ...args
      );

delayedRecursively(countdown,500)(10);

const sampleTree = {
    left: {
        left: {
            left: null,
            value: 5,
            right: null,
        },
        value: 2,
        right: {
            left: null,
            value: 5,
            right: null,
        },
    },
    value: 6,
    right: null,
};

const sumTree = (recurse, t, resolve) => {
    if (t === null) resolve(0);
    else
        recurse(t.left, (n1) =>
            recurse(t.right, (n2) =>
                resolve(n1 + t.value + n2)));
}

delayedRecursively(sumTree, 100)(sampleTree, (n) => console.log(`Sum is ${n}`));
