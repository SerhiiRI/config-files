;;; INSTRUCTION

;;; for showing all fonts just eval from emacs
;;; lvl function `expand-list-result-sexp' on
;;; show-all-fonts

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun expand-list-result-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (if (listp value)
	(mapcar (lambda (v)
		  ;; (kill-sexp -1)
		  (newline)
		  (insert (format "%S" v))
		  (end-of-line))
		value))))

(defun show-all-fonts ()
  (mapcar (lambda (font) `(set-frame-font ,font))
	  (font-family-list)))


