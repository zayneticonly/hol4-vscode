import { workspace } from 'vscode';
export const EXTENSION_ID = 'oskarabrahamsson.hol4-mode';
export const KERNEL_ID = 'hol4';

/** Log a message with the 'hol-mode' prefix. */
export function log(message: string): void {
    console.log(`--- hol-mode: ${message}`);
}

/** Log an error with the 'hol-mode' prefix. */
export function error(message: string): void {
    console.error(`!!! hol-mode: Error: ${message}`);
}

export function holdir(): string | undefined {
    return workspace.getConfiguration('hol4-mode').get<string>('holdir');
}

/** Execute an async fn such that any concurrent calls block until the previous calls finish. */
export function disallowConcurrency<T>(fn: (arg: T) => Promise<void>): (arg: T) => Promise<void> {
    let inprogressPromise = Promise.resolve()
    return (arg) => {
        inprogressPromise = inprogressPromise.then(() => fn(arg))
        return inprogressPromise
    }
};

export function partitionPoint(len: number, pred: (i: number) => boolean) {
    let start = 0;
    while (0 < len) {
        const half = len / 2 | 0;
        const middle = start + half;
        if (pred(middle)) {
            start = middle + 1;
            len -= half + 1;
        } else {
            len = half;
        }
    }
    return start;
}

export function pluralize(n: number, stem: string, s: string = 's') {
    return `${n} ${n == 1 ? stem : stem + s}`;
}
