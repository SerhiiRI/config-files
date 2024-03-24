;; Requirements 
(require 'dash)
(setq mark-param-list [(44 "2") (53 "3") (62 "3+") (71 "4") (80 "4+") (89 "5") (100 "5+")])

(defun -get-max-mark (mark-list)
  (let ((dmX (-partition 2 mark-list))
	(summary 0.0))
      (dolist (dm dmX)
	(-let (((description mark) dm))
	      (let ((def-mark (if (not (listp mark)) 1 (car (cdr mark)))))
		(setq summary (+ summary def-mark)))))
      (/ summary (length dmX))))

(defun -get-mark-value (max-mark-value mark-value)
  (let ((mp-list (mapcar (lambda (mp-pair) (list (/ (* max-mark-value (car mp-pair)) 100) (cadr mp-pair))) mark-param-list)))
    (seq-find (lambda (mp-pair) (>= (car mp-pair) mark-value)) mp-list "2")))

(defun exame (mark-list)
  (let ((dmX (-partition 2 mark-list))
	(summary 0.0))
    (dolist (dm dmX)
      (-let (((description mark) dm))
	(let ((def-mark (if (not (listp mark)) mark (* (car mark) (car (cdr mark))))))
	  (setq summary (+ summary def-mark)))))
    (let* ((max-mark-value (-get-max-mark mark-list))
	   (mark-value (/ summary (length dmX)))
	   (result-mark (-get-mark-value max-mark-value mark-value)))
      (format "(Mark Point %.2f/%.2f) (Finally %s)"
	      mark-value
	      max-mark-value
	      (cadr result-mark)))))

(setq REANSWER-LEVEL 0.9)
(defun reanswer (mark-list)
  (let ((dmX (-partition 2 mark-list)))
    (seq-filter (lambda (dm) 
		  (-let (((description mark) dm))
		    (if (not (listp mark)) (> REANSWER-LEVEL mark) (> REANSWER-LEVEL (car mark)))))
		dmX)))

(defun do-exam (mark-list)
  (princ (format "Оцінки: %s" (exame mark-list)))
  (princ "\nTo reanswer: \n")
  (mapcar (lambda (mp-pair)
	    (princ (format "\t%s (%.1f/1)\n" (car mp-pair) (if (listp (cadr mp-pair))
							       (car (cadr mp-pair))
							     (cadr mp-pair))))) (reanswer mark-list))
  nil)


(defun do-exam-string (mark-list)
  (format "%s\n%s\n%s"
	  (format "Оцінки: %s" (exame mark-list))
	  (format "To reanswer: \n")
	  (string-join (mapcar (lambda (mp-pair)
				 (format "\t%s (%.1f/1)"
					 (car mp-pair)
					 (if (listp (cadr mp-pair))
					     (car (cadr mp-pair))
					   (cadr mp-pair)))) (reanswer mark-list))
		       "\n")))


;; (do-exam-string '("Hello World" (0.5 0.2)
;; 		  "Tree" (0.9 1.0)
;; 		  "Filter array" (1 1.0)
;; 		  "Htmlizer" (0.3 1.0)))

;; (do-exam-insert
;;  '("Система оціювання" 0.5
;;    "ваіофавіфщшо ощшавоіф щавіфш щш32о4 " (1 0.5)
;;    "авіф0ао т23щшкт" (0.2 1)
;;    "bliat" (1 1)
;;    "bliat" (1 1)))







