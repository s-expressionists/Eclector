(cl:in-package #:eclector.reader)

(macrolet
    ((define-reporter (((condition-var condition-specializer) stream-var)
                       &body body)
       `(defmethod acclimation:report-condition
            ((,condition-var ,condition-specializer)
             ,stream-var
             (language acclimation:english))
          ,@body)))

  ;; For the following reporters, we use the DELIMITER slot instead of
  ;; a fixed character in the report since the reader macros may have
  ;; been installed on non-default (sub-) characters.
  (define-reporter ((condition unterminated-list) stream)
    (let ((delimiter (delimiter condition)))
      (format stream "While reading list, expected ~:c when input ~
                      ended."
              delimiter)))

  (define-reporter ((condition unterminated-vector) stream)
    (let ((delimiter (delimiter condition)))
      (format stream "While reading vector, expected ~:c when input ~
                      ended."
                delimiter)))

  (define-reporter ((condition unterminated-string) stream)
    (let ((delimiter (delimiter condition)))
      (format stream "While reading string, expected ~:c when input ~
                      ended."
              delimiter)))

  (define-reporter ((condition unterminated-block-comment) stream)
    (let ((delimiter (delimiter condition)))
      (format stream "While reading block comment, expected ~:c ~:c ~
                      when input ended."
              delimiter #\#)))

  (define-reporter ((condition read-object-type-error) stream)
    (format stream "The read object ~s is not of the required type ~s."
            (type-error-datum condition)
            (type-error-expected-type condition)))

  (define-reporter ((condition invalid-context-for-backquote) stream)
    (format stream "Backquote has been used in a context that does not ~
                    permit it (like #C(1 `,2))."))

  (define-reporter ((condition comma-not-inside-backquote) stream)
    (format stream "~:[Comma~;Splicing comma~] not inside backquote."
            (at-sign-p condition)))

  (define-reporter ((condition object-must-follow-comma) stream)
    (format stream "An object must follow a~:[~; splicing~] comma."
            (at-sign-p condition)))

  (define-reporter ((condition unquote-splicing-in-dotted-list) stream)
    (format stream "Splicing unquote at end of list (like a . ,@b)."))

  (define-reporter ((condition unquote-splicing-at-top) stream)
    (format stream "Splicing unquote as backquote form (like `,@foo)."))

  (define-reporter ((condition too-many-dots) stream)
    (format stream "A token consisting solely of multiple dots is ~
                    illegal."))

  (define-reporter ((condition invalid-context-for-consing-dot) stream)
    (format stream "A consing dot appeared in an illegal position."))

  (define-reporter ((condition object-must-follow-consing-dot) stream)
    (format stream "An object must follow a consing dot."))

  (define-reporter ((condition multiple-objects-following-consing-dot) stream)
    (format stream "Only a single object can follow a consing dot."))

  (define-reporter ((condition invalid-context-for-right-parenthesis) stream)
    (format stream "Unmatched close parenthesis."))

  (define-reporter ((condition package-does-not-exist) stream)
    (format stream "Package named ~s does not exist."
            (desired-package-name condition)))

  (define-reporter ((condition symbol-does-not-exist) stream)
    (format stream "Symbol named ~s not found in the ~a package."
            (desired-symbol-name condition)
            (package-name (desired-symbol-package condition))))

  (define-reporter ((condition symbol-is-not-external) stream)
    (format stream "Symbol named ~s is not external in the ~a package."
            (desired-symbol-name condition)
            (package-name (desired-symbol-package condition))))

  (define-reporter ((condition invalid-constituent-character) stream)
    (let ((char (aref (token condition) 0)))
       (format stream "The character ~:[named ~A~*~;~*~C~] must not ~
                       occur in a symbol as it is an invalid ~
                       constituent."
               (graphic-char-p char) (char-name char) char)))

  (define-reporter ((condition symbol-name-must-not-be-only-package-markers) stream)
    (format stream "Symbol name without any escapes must not consist ~
                    solely of package markers (: characters)."))

  (define-reporter ((condition symbol-name-must-not-end-with-package-marker) stream)
    (format stream "Symbol name must not end with a package ~
                    marker (the : character)."))

  (define-reporter ((condition two-package-markers-must-be-adjacent) stream)
    (format stream "If a symbol token contains two package markers, ~
                    they must be adjacent as in package::symbol."))

  (define-reporter ((condition two-package-markers-must-not-be-first) stream)
    (format stream "A symbol token must not start with two package ~
                    markers as in ::name."))

  (define-reporter ((condition symbol-can-have-at-most-two-package-markers) stream)
    (format stream "A symbol token must not contain more than two ~
                    package markers as in package:::symbol or ~
                    package::first:rest."))

  (define-reporter ((condition uninterned-symbol-must-not-contain-package-marker) stream)
    (format stream "A symbol token following #: must not contain a ~
                    package marker."))

  (define-reporter ((condition numeric-parameter-supplied-but-ignored) stream)
    (format stream "Dispatch reader macro ~a was supplied with a ~
                    numeric parameter it does not accept."
            (macro-name condition)))

  (define-reporter ((condition numeric-parameter-not-supplied-but-required) stream)
    (format stream "Dispatch reader macro ~a requires a numeric ~
                    parameter, but none was supplied."
            (macro-name condition)))

  (define-reporter ((condition read-time-evaluation-inhibited) stream)
    (format stream "Cannot evaluate expression at read-time because ~s ~
                    is false."
            '*read-eval*))

  (define-reporter ((condition read-time-evaluation-error) stream)
      (let ((expression (expression condition))
            (original-condition (original-condition condition)))
        (format stream "Read-time evaluation of expression ~s signaled ~
                        ~s: ~a"
                expression (type-of original-condition) original-condition)))

  (define-reporter ((condition unknown-character-name) stream)
    (format stream "Unrecognized character name: ~s" (name condition)))

  (define-reporter ((condition digit-expected) stream)
    (format stream "~:c is not a digit in base ~d."
            (character-found condition) (base condition)))

  (define-reporter ((condition invalid-radix) stream)
    (format stream "~d is too ~:[big~;small~] to be a radix."
            (radix condition) (< (radix condition) 2)))

  (define-reporter ((condition invalid-default-float-format) stream)
    (format stream "~a is not a valid ~a."
            (float-format condition) 'cl:*read-default-float-format*))

  (define-reporter ((condition too-many-elements) stream)
    (format stream "~a was specified to have length ~d, but ~d ~
                    element~:P ~:*~[were~;was~:;were~] found."
            (array-type condition)
            (expected-number condition)
            (number-found condition)))

  (define-reporter ((condition no-elements-found) stream)
    (format stream "~a was specified to have length ~d, but no ~
                    elements were found."
            (array-type condition) (expected-number condition)))

  (define-reporter ((condition incorrect-initialization-length) stream)
    (format stream "~a was specified to have length ~d along the ~:R ~
                    axis, but provided initial-contents don't ~
                    match:~%~a"
            (array-type condition)
            (expected-length condition)
            (1+ (axis condition))
            (datum condition)))

;;; Sharpsign S conditions

  (define-reporter ((condition non-list-following-sharpsign-s) stream)
    (format stream "A proper list must immediately follow #S."))

  (define-reporter ((condition no-structure-type-name-found) stream)
    (format stream "A symbol naming a structure type must be the first ~
                    element of the list following #S."))

  (define-reporter ((condition structure-type-name-is-not-a-symbol) stream)
    (format stream "~S should designate a structure type but is not a ~
                    symbol."
            (type-error-datum condition)))

  (define-reporter ((condition slot-name-is-not-a-symbol) stream)
    (format stream "~S should designate a structure slot but is not ~
                    a symbol."
            (type-error-datum condition)))

  (define-reporter ((condition no-slot-value-found) stream)
    (format stream "A slot value form must follow the slot name ~S."
            (slot-name condition)))

  (define-reporter ((condition feature-expression-type-error) stream)
    (format stream "Feature expression is not of type ~a:~%~a"
            (type-error-expected-type condition) (type-error-datum condition)))

  (define-reporter ((condition single-feature-expected) stream)
    (format stream "Bad feature expression- found multiple features ~
                    when only one was expected:~%~a"
            (features condition)))

  (define-reporter ((condition sharpsign-invalid) stream)
    (format stream "~:c is not a valid subchar for the # dispatch macro."
            (character-found condition)))

  (define-reporter ((condition sharpsign-equals-label-defined-more-than-once) stream)
    (format stream "Label ~d defined more than once."
            (label condition)))

  (define-reporter ((condition sharpsign-equals-only-refers-to-self) stream)
    (format stream "Label ~d is defined as a reference to itself."
            (label condition)))

  (define-reporter ((condition sharpsign-sharpsign-undefined-label) stream)
    (format stream "Reference to undefined label #~d#." (label condition)))

  ) ; MACROLET DEFINE-REPORTER
