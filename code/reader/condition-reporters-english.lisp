(cl:in-package #:eclector.reader)

;;; FIXME: most of these should have file position, etc. if available.
;;; Put that stuff in this method.

(defmethod acclimation:report-condition :after
    ((condition reader-error)
     stream
     (language acclimation:english))
  (format stream "~&~% Stream: ~a" (stream-error-stream condition)))

;; TODO: invalid-context-for-backquote

(defmethod acclimation:report-condition
    ((condition comma-not-inside-backquote)
     stream
     (language acclimation:english))
  (format stream "Comma not inside backquote."))

(defmethod acclimation:report-condition
    ((condition unquote-splicing-in-dotted-list)
     stream
     (language acclimation:english))
  (format stream "Splicing unquote at end of list (like a . ,@b)."))

(defmethod acclimation:report-condition
    ((condition unquote-splicing-at-top)
     stream
     (language acclimation:english))
  (format stream "Splicing unquote as backquote form (like `,@foo)."))

;; TODO: consing dot, right paren

(defmethod acclimation:report-condition
    ((condition invalid-context-for-right-parenthesis)
     stream
     (language acclimation:english))
  (format stream "Unmatched close parenthesis."))

(defmethod acclimation:report-condition
    ((condition sub-char-must-not-be-a-decimal-digit)
     stream
     (language acclimation:english))
  (format stream "~:c cannot be defined as a dispatch macro sub-character, as it is a decimal digit."
          (sub-char condition)))

(defmethod acclimation:report-condition
    ((condition char-must-be-a-dispatching-character)
     stream
     (language acclimation:english))
  (format stream "~:c cannot have a dispatch macro set for it, as it has not been defined as a dispatch macro~@
                  (as by ~a)"
          (disp-char condition) 'cl:make-dispatch-macro-character))

(defmethod acclimation:report-condition
    ((condition symbol-does-not-exist)
     stream
     (language acclimation:english))
  (format stream "Symbol named ~s not found in the ~a package."
          (desired-symbol-name condition) (package-name (desired-symbol-package condition))))

(defmethod acclimation:report-condition
    ((condition symbol-is-not-external)
     stream
     (language acclimation:english))
  (format stream "Symbol named ~s is not external in the ~a package."
          (desired-symbol-name condition) (package-name (desired-symbol-package condition))))

;; TODO: package marker stuff

;; unknown macro sub character not used yet

(defmethod acclimation:report-condition
    ((condition numeric-parameter-supplied-but-ignored)
     stream
     (language acclimation:english))
  (format stream "Dispatch reader macro ~a was supplied with a numeric parameter it does not accept."
          (macro-name condition)))

(defmethod acclimation:report-condition
    ((condition numeric-parameter-not-supplied-but-required)
     stream
     (language acclimation:english))
  (format stream "Dispatch reader macro ~a requires a numeric parameter, but none was supplied."
          (macro-name condition)))

;; read time evaluation inhibited not used yet

(defmethod acclimation:report-condition
    ((condition unknown-character-name)
     stream
     (language acclimation:english))
  (format stream "Unrecognized character name: ~s" (name condition)))

(defmethod acclimation:report-condition
    ((condition digit-expected)
     stream
     (language acclimation:english))
  (format stream "~:c is not a digit in base ~d."
          (character-found condition) (base condition)))

(defmethod acclimation:report-condition
    ((condition invalid-radix)
     stream
     (language acclimation:english))
  (format stream "~d is too ~:[big~;small~] to be a radix."
          (radix condition) (< (radix condition) 2)))

(defmethod acclimation:report-condition
    ((condition invalid-default-float-format)
     stream
     (language acclimation:english))
  (format stream "~a is not a valid ~a." (float-format condition) 'cl:*read-default-float-format*))

(defmethod acclimation:report-condition
    ((condition too-many-elements)
     stream
     (language acclimation:english))
  (format stream "Bit vector was specified to have length ~d, but ~d elements were found."
          (expected-number condition) (number-found condition)))

(defmethod acclimation:report-condition
    ((condition no-elements-found)
     stream
     (language acclimation:english))
  (format stream "Bit vector was specified to have length ~d, but no elements were found."
          (expected-number condition)))

(defmethod acclimation:report-condition
    ((condition incorrect-initialization-length)
     stream
     (language acclimation:english))
  (format stream "Array was specified to have length ~d, but provided initial-contents don't match:~%~a"
          (expected-length condition) (datum condition)))

(defmethod acclimation:report-condition
    ((condition single-feature-expected)
     stream
     (language acclimation:english))
  (format stream "Bad feature expression- found multiple features when only one was expected:~%~a"
          (features condition)))

(defmethod acclimation:report-condition
    ((condition sharpsign-invalid)
     stream
     (language acclimation:english))
  (format stream "~:c is not a valid subchar for the # dispatch macro."
          (character-found condition)))

(defmethod acclimation:report-condition
    ((condition sharpsign-equals-label-defined-more-than-once)
     stream
     (language acclimation:english))
  (format stream "Sharpsign reader macro label ~d defined more than once."
          (label condition)))

(defmethod acclimation:report-condition
    ((condition sharpsign-sharpsign-undefined-label)
     stream
     (language acclimation:english))
  (format stream "Reference to undefined label #~d#." (label condition)))
