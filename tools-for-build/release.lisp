#!/usr/bin/env -S cl -s alexandria -s split-sequence -s inferior-shell -s eclector-concrete-syntax-tree -L tools-for-build/read-changes.lisp -L tools-for-build/write-news.lisp

(cl:defpackage #:eclector.tools-for-build.release
  (:use
   #:cl
   #:alexandria
   #:split-sequence)

  (:local-nicknames
   (#:shell   #:inferior-shell)
   (#:changes #:eclector.tools-for-build.read-changes)
   (#:news    #:eclector.tools-for-build.write-news)))

(cl:in-package #:eclector.tools-for-build.release)

;;; Git actions

(defun tag (name)
  (shell:run `("git" "tag" ,name)))

(defun checkout (name)
  (shell:run `("git" "checkout" ,name)))

(defun commit (message)
  (shell:run `("git" "commit" "-a" "-m" ,message)))

;;;

(defun read-version ()
  (let* ((current/string (uiop:read-file-form "version.sexp"))
         (current/list   (mapcar #'parse-integer
                                 (split-sequence #\. current/string))))
    (assert (= 3 (length current/list)))
    (values current/list current/string)))

(defun bump-version (new-version)
  (with-output-to-file (stream "version.sexp" :if-exists :supersede)
    (format stream "\"~{~A~^.~}\"~%" new-version)))

(defun add-release-date ()
  (let* ((file    "changes.sexp")
         (content (read-file-into-string file))
         (cst     (with-input-from-string (stream content)
                    (eclector.concrete-syntax-tree:read stream)))
         (release (cst:second cst))
         (version (cst:raw (cst:second release)))
         (date    (cst:third release)))
    (destructuring-bind (start . end) (cst:source date)
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (get-universal-time))
        (declare (ignore second minute hour))
        (unless (null (cst:raw date))
          (error "~@<There already is a release date in the entry for ~
                  release ~A.~@:>"
                 version))
        (format *trace-output* "Finalizing release notes for version ~A, ~
                                date is ~D-~2,'0D-~2,'0D~%"
                version year month day)
        (setf content (format nil "~A~
                                   \"~D-~2,'0D-~2,'0D\"~
                                   ~A"
                              (subseq content 0 start)
                              year month day
                              (subseq content end)))
        (write-string-into-file content file :if-exists :supersede)))))

(defun add-release (version)
  (let* ((file    "changes.sexp")
         (content (read-file-into-string file))
         (cst     (with-input-from-string (stream content)
                    (eclector.concrete-syntax-tree:read stream)))
         (release (cst:second cst))
         (start   (car (cst:source release))))
    (format *trace-output* "Adding empty change log section for ~A~%" version)
    (setf content (format nil "~A~
                               ~(~S~)~@
                               ~@
                               ~1@T~A"
                          (subseq content 0 start)
                          `(:release ,version nil)
                          (subseq content start)))
    (write-string-into-file content file :if-exists :supersede)))

(defun release ()
  (let* ((this-version/list   (read-version))
         (next-version/list   (destructuring-bind (major minor commit)
                                  this-version/list
                                (list major (1+ minor) commit)))
         (this-release/string (format nil "~{~D~^.~}"
                                      (subseq this-version/list 0 2)))
         (next-release/string (format nil "~{~D~^.~}"
                                      (subseq next-version/list 0 2))))
    ;; Add release date to current section in changes.sexp and write
    ;; release notes.
    (add-release-date)
    (let ((changes (changes:read-changes "changes.sexp")))
      (flet ((write-release-notes (release-notes-file &rest args)
               (format *trace-output* "Writing release notes ~A~%" release-notes-file)
               (apply #'news:write-news changes release-notes-file args)))
        (write-release-notes "NEWS" :plaintext)
        (write-release-notes "NEWS.md" :markdown :count 1)))
    (commit (format nil "Add date to ~A release in changes.sexp~%"
                    this-release/string))
    ;; Create release tag.
    (let ((tag-name (format nil "release-~A" this-release/string)))
      (format *trace-output* "Creating tag ~S~%" tag-name)
      (tag tag-name))

    ;; Bump version in version-string.sexp and create new section in
    ;; changes.sexp
    (format *trace-output* "Bumping version ~{~A~^.~} → ~{~A~^.~}~%"
            this-version/list next-version/list)
    (bump-version next-version/list)
    (add-release next-release/string)
    (commit (format nil "Version bump ~A -> ~A~%"
                    this-release/string next-release/string))))

(release)
