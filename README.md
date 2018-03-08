# Eclector: A portable and extensible Common Lisp Reader

## Introduction ##

The `eclector` system provides a portable implementation of a reader
following the Common Lisp specification.

## Tutorial ##

### Basics ###

In the simplest case, the eclector reader can be used like any Common
Lisp reader:

``` lisp
(with-input-from-string (stream "(1 2 3)")
  (eclector.reader:read stream))
```

### Concrete Syntax Trees ###

The `eclector.concrete-syntax-tree` system provides a variant of the
`eclector` reader that produces instances of the concrete syntax tree
classes provided by the [concrete syntax tree library]:

``` lisp
(with-input-from-string (stream "(1 2 3)")
  (eclector.concrete-syntax-tree:cst-read stream))
```

[concrete syntax tree library]: https://github.com/robert-strandh/Concrete-Syntax-Tree
