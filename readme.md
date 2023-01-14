Derives an enum of same-variant pairs of another enum.

# Installation

```sh
cargo add samevariant
```

# Use

Define an enum and use this derive macro:

```
#[samevariant(pub ABCSameVariant)]
pub enum ABC {
    A,
    B,
    C(i32),
}
```

It generates a new enum that looks like:
```
enum ABCSameVariant<'l> {
    Nonmatching(&'l ABC, &'l ABC),
    A,
    B,
    C(&'l i32, &'l i32),
}

impl ABCSameVariant {
    fn pairs<'l>(a: &'l ABC, b: &'l ABC) -> ABCSameVariant<'l> {
        ...
    }
}
```

If the base enum has a variant `Nonmatching` it will be prefixed by the original enum name (like `ABCNonmatching`).

Only simple and tuple variants are supported, struct-like variants will result in an error.
