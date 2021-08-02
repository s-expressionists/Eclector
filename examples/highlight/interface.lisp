(cl:in-package #:eclector.examples.highlight)

(defun make-minimal-client (&key (stream *standard-output*))
  (make-instance 'eclector.examples.highlight.render::minimal-html-client :stream stream))

(defun make-linking-client (input &key (stream *standard-output*))
  (make-instance 'eclector.examples.highlight.render::linking-html-client :input input :stream stream))

(defun highlight (input-string &key (package "COMMON-LISP-USER")
                                    (read-client nil                   read-client-supplied-p)
                                    (client      (make-minimal-client)))
  (multiple-value-bind (cst errors)
      (apply #'read-stuff input-string :package package
             (when read-client-supplied-p
               (list :client read-client)))
    (eclector.examples.highlight.render:render client input-string cst errors)))

(defun highlight-string (string &key package client)
  (with-output-to-string (stream)
    (reinitialize-instance client :stream stream)
    (apply #'highlight string :client client (when package
                                               (list :package package)))))

(defun process (input output &key (client (make-linking-client input)))
  (a:with-output-to-file (stream output :if-exists :supersede)
    (reinitialize-instance client :stream stream)
    (apply #'highlight input (when client
                               (list :client client)))))
