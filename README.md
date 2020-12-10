# rsmonkey

## REPL

```bash
cargo run -p repl
```

or

```bash
cargo run -p repl input.txt
```

## Writing Code

```js
// Assign
let num = 100;
puts(num); // 100

// String
let str = "Hello" + "World";
puts(str); // "Hello World"

// Boolean
let bool = true;
puts(bool, false); // true, false

// Array
let arr = [1, 2, 3];
puts(arr, arr[1]); // [1, 2, 3] 2

let pushedArr = push(arr, 100);
puts(pushedArr); // [1, 2, 3, 100]

let restArr = rest(arr);
puts(restArr); // [2, 3, 100]

let firstVal = first(arr);
puts(firstVal); // 1

let lastVal = last(arr);
puts(lastVal); // 3

// Hash
let hash = { "foo": "bar", 1: 2, true: 3, false: 4 };
puts(hash, hash[true]); // { "foo": "bar", 1: 2, true: 3, false: 4 } 3

let insertedHash = insert(hash, "key", "value");
puts(insertedHash); // { "foo": "bar", 1: 2, true: 3, false: 4, "key": "value" }

let removedHash = remove(hash, "foo");
puts(removedHash); // { 1: 2, true: 3, false: 4, "key": "value" }

// Length
puts(len(str), len(arr), len(hash)); // 11 3 4

// Function
let f = fn() {
  puts("Hello Function");
};
f(); // "Hello Function"
```
