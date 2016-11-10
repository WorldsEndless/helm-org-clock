;; Modification to the org clock functions to allow C-u org-clock-in to use a completing-read if `org-select-task-completing' is true
;; Tory S. Anderson, 2015

(setq org-select-task-completing t)

(defun org-clock-select-task (&optional prompt)
  "Select a task that was recently associated with clocking. 
If `org-select-task-completing', use a completing-read to select. 
Otherwise use the custom orgmode menus."
  (interactive)
  (let (och chl sel-list rpl (i 0) s)
    ;; Remove successive dups from the clock history to consider
    (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
	  org-clock-history)
    (setq och (reverse och) chl (length och))
    (if (zerop chl)
	(user-error "No recent clock")
      ;; Completing read (e.g. Helm)
      (if org-select-task-completing
	  (let ((crlist (org-select-task-table och i)))
	    (cdr (gethash (completing-read "Clock Task Select:" crlist nil t) crlist)))
	;; Orgmode menus
	(save-window-excursion
	  (org-switch-to-buffer-other-window
	   (get-buffer-create "*Clock Task Select*"))
	  (erase-buffer)
	  (when (marker-buffer org-clock-default-task)
	    (insert (org-add-props "Default Task\n" nil 'face 'bold))
	    (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
	    (push s sel-list))
	  (when (marker-buffer org-clock-interrupted-task)
	    (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
	    (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
	    (push s sel-list))
	  (when (org-clocking-p)
	    (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
	    (setq s (org-clock-insert-selection-line ?c org-clock-marker))
	    (push s sel-list))
 	  (insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
	  (mapc
	   (lambda (m)
	     (when (marker-buffer m)
	       (setq i (1+ i)
		     s (org-clock-insert-selection-line
			(if (< i 10)
			    (+ i ?0)
			  (+ i (- ?A 10))) m))
	       (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
	       (push s sel-list)))
	   och)
	  (run-hooks 'org-clock-before-select-task-hook)
	  (goto-char (point-min))
	  ;; Set min-height relatively to circumvent a possible but in
	  ;; `fit-window-to-buffer'
	  (fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
	  (message (or prompt "Select task for clocking:"))
	  (setq cursor-type nil rpl (read-char-exclusive))
	  (cond
	   ((eq rpl ?q) nil)
	   ((eq rpl ?x) nil)
	   ((assoc rpl sel-list) (message (cdr (assoc rpl sel-list))))
	   (t (user-error "Invalid task choice %c" rpl)))))))
  )

(defun org-clock-get-selection-line (marker)
  "Return the string for a line for the clock selection list, 
for use in a completing-read"
  (when (marker-buffer marker)
    (let (file cat task heading prefix)
      (with-current-buffer (org-base-buffer (marker-buffer marker))
	(save-excursion
	  (save-restriction
	    (widen)
	    (ignore-errors
	      (goto-char marker)
	      (setq file (buffer-file-name (marker-buffer marker))
		    cat (org-get-category)
		    heading (org-get-heading 'notags)
		    prefix (save-excursion
			     (org-back-to-heading t)
			     (looking-at org-outline-regexp)
			     (match-string 0))
		    task (substring
			  (org-fontify-like-in-org-mode
			   (concat prefix heading)
			   org-odd-levels-only)
			  (length prefix)))))))
      (when (and cat task)
	(format "%s\t%s" cat task)
	))))

(defun org-select-task-table (och i)
  "Return a hash table equivalent to the 
regular clock task selection list, associating 
task-name with (char, marker)"
  (let ((crlist (make-hash-table :test #'equal)))
    (mapc (lambda (l) (puthash
		       (org-clock-get-selection-line l)
		       (cons (if (< i 10)
				 (+ i ?0)
			       (+ i (- ?A 10)))
			     l) crlist)) och)
    crlist
    ))
