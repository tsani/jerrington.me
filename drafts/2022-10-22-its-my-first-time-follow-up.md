---
title: "It's my first time: follow-up"
---

```typescript
function runningAtMostOnce(
    f: () => Promise<void>,
    onAlreadyRunning?: () => void,
): () => Promise<void> {
    let enter: () => Promise<void>;
    const alreadyRunning = () => Promise.resolve(onAlreadyRunning?.());
    const notAlreadyRunning = () => {
        enter = alreadyRunning;
        return f().finally(() => {
            enter = notAlreadyRunning;
        });
    };
    enter = notAlreadyRunning;
    return () => enter();
```
