# Eclector: A portable and extensible Common Lisp Reader

## Introduction ##

The eclector systems provides a portable implementation of a reader
following the Common Lisp specification.

## Tutorial ##

In the simplest case, the eclector reader can be used like any Common
Lisp reader:

```lisp
(with-input-from-string (stream "(1 2 3)")
  (eclector.reader:read stream))
```
