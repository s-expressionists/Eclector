(cl:in-package #:eclector.reader)

(defmacro update-escape-ranges
    (index escape-range-place remaining-escape-ranges-place)
  (alexandria:once-only (index)
    `(loop while (and (not (null ,escape-range-place))
                      (>= ,index (cdr ,escape-range-place)))
           do (pop ,remaining-escape-ranges-place)
              (setf ,escape-range-place (first ,remaining-escape-ranges-place))
           finally (return (and (not (null ,escape-range-place))
                                (<= (car ,escape-range-place) ,index))))))

(defun convert-according-to-readtable-case (token escape-ranges)
  (macrolet
      ((do-token ((index-var escapep-var) &body body)
         `(loop with remaining-escape-ranges = escape-ranges
                with escape-range = (first remaining-escape-ranges)
                for ,index-var below (length token)
                for ,escapep-var = (update-escape-ranges
                                    ,index-var
                                    escape-range remaining-escape-ranges)
                do (progn ,@body)))
       (change-case (string-function char-function)
         `(cond
            ;; First common and easy case: no escapes. Change the case
            ;; of all characters in TOKEN at once.
            ((null escape-ranges)
             (setf token (,string-function token)))
            ;; Second common and easy case: all characters are
            ;; escaped. Just return TOKEN.
            ((and (null (cdr escape-ranges))
                  (zerop (car (first escape-ranges)))
                  (= (length token) (cdr (first escape-ranges))))
             nil)
            (t
             (do-token (i escapep)
               (unless escapep
                 (setf (aref token i) (,char-function (aref token i)))))))))
    (ecase (eclector.readtable:readtable-case *readtable*)
      (:upcase
       (change-case nstring-upcase char-upcase))
      (:downcase
       (change-case nstring-downcase char-downcase))
      (:preserve
       nil)
      (:invert
       (let ((upper-case-p nil)
             (lower-case-p nil))
         (if (null escape-ranges)
             (setf upper-case-p (find-if #'upper-case-p token)
                   lower-case-p (find-if #'lower-case-p token))
             (block nil
               (do-token (i escapep)
                 (unless escapep
                   (let ((char (aref token i)))
                     (cond ((upper-case-p char)
                            (setf upper-case-p t)
                            (when lower-case-p
                              (return)))
                           ((lower-case-p char)
                            (setf lower-case-p t)
                            (when upper-case-p
                              (return)))))))))
         (cond ((not upper-case-p)
                (change-case nstring-upcase char-upcase))
               ((not lower-case-p)
                (change-case nstring-downcase char-downcase))))))
    token))
