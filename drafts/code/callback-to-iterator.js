const timeout = (millis) =>
    new Promise((resolve) => setTimeout(() => resolve(), millis));

/** Starts an async operation that invokes callback with successive integers delaying by `delay`
 * after each call. */
function exampleWithCallback(limit, delay, callback) {
    (async () => {
        for (let i = 0; i < limit; i++) {
            callback(i, false);
            await timeout(delay);
        }
        callback(undefined, true);
    })();
}

/** Connects a pull-based stream to a push-based stream.
 * Pull-based streams are represented as AsyncIterator, whereas a push-based stream is represented
 * by callback calling. */
class CallbackIterable {
    constructor() {
        this.pushQueue = [];
        this.pullQueue = [];
        this.closed = false;
    }

    connect(f) {
        f((x, finished) => finished ? this.close() : this.push(x));
    }

    close() {
        this.closed = true;
        for (const [resolve, reject] of this.pullQueue) {
            resolve({ done: true });
        }
    }

    push(v) {
        if (this.closed) throw new Error('cannot push to a closed stream');

        if (this.pullQueue.length) {
            const [[resolve, reject]] = this.pullQueue.splice(0, 1);
            resolve({ value: v });
        } else {
            this.pushQueue.push(v);
        }
    }

    pull(resolve, reject) {
        if (this.closed) resolve({ done: true });

        if (this.pushQueue.length) {
            const [v] = this.pushQueue.splice(0, 1);
            setImmediate(() => resolve({ value: v }));
        } else {
            this.pullQueue.push([resolve, reject]);
        }
    }

    [Symbol.asyncIterator]() {
        return {
            next: () => new Promise(
                (resolve, reject) => this.pull(resolve, reject),
            ),
        };
    }
}

async function example() {
    const stream = new CallbackIterable();
    stream.connect((f) => exampleWithCallback(100, 50, f));
    for await (const x of stream) {
        console.log(x);
    }
    console.log('did it!');
}

example();
