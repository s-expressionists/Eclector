## Introduction

This directory contains source files each of which implements a syntax
extension.

## Extended package prefix

Based on an
[SBCL extension](https://sbcl.org/manual/index.html#Extended-Package-Prefix-Syntax),
the syntax `PACKAGE::OBJECT` can be used to read an object in a
package other than the current one.

Examples:

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.extended-package-prefix:extended-package-prefix-syntax-mixin)))
  (read-from-string "cl-user::(foo bar 1)"))
; => (cl-user::foo cl-user::bar 1)
```

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.extended-package-prefix:extended-package-prefix-syntax-mixin)))
  (read-from-string "cl-user::#(foo)"))
; => #(cl-user::foo)
```

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.extended-package-prefix:extended-package-prefix-syntax-mixin)))
  (read-from-string "cl-user::;foo
bar"))
; => cl-user::bar
```
