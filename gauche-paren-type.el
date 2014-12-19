;; -*- lexical-binding: t -*-
(require 'jit-lock)

(defvar-local gauche-paren-type-info '())

(defface gauche-paren-type-proc-paren-face
  '((t :foreground "#f0908a"))
  "Face for procedure call parentheses")

(defface gauche-paren-type-macro-paren-face
  '((t :foreground "#a53d92"
       :weight bold))
  "Face for macro call parentheses")

(defface gauche-paren-type-other-paren-face
  '((t :foreground "#63c8f2"))
  "Face for other parentheses")

(defun gauche-paren-type-read-info ()
  "Read paren type information for the current byffer
by invoking an external script."
  (interactive)
  (setq gauche-paren-type-info
        (let ((outbuf (generate-new-buffer " *temp*")))
          (unwind-protect
              (let ((ret (call-process-region
                          (point-min)
                          (point-max)
                          "dump-call-type.scm"
                          nil
                          (list outbuf nil)
                          nil
                          (format "%s" (buffer-file-name)))))
                (cond ((zerop ret)
                       (with-current-buffer outbuf
                         (goto-char (point-min)))
                       (read outbuf))
                      (t
                       nil)))
            (kill-buffer outbuf)))))

(defun gauche-paren-type--info-line (info)
  (nth 1 info))

(defun gauche-paren-type-info-car (info)
  (nth 2 info))

(defun gauche-paren-type-info-type (info)
  (nth 3 info))

(defun gauche-paren-type-highlight-current-list (face)
  (unless (looking-at "[[(]")
    (error "not at a start of list"))
  (save-excursion
    (let ((prop `(font-lock-face ,face rear-nonsticky t)))
      (let ((pos (point)))
        (add-text-properties pos (1+ pos) prop))
      (forward-sexp)
      (let ((pos (point)))
        (add-text-properties (1- pos) pos prop)))))

(defmacro gauche-paren-type-gen-reader-macro-condition (car &rest pairs)
  (let (($car (gensym)))
    `(let ((,$car ,car))
       (or
        ,@(mapcar (lambda (pair)
                    (let ((sym (car pair))
                          (name (cadr pair)))
                      `(and (equal ,$car ,name)
                            (looking-back ,(format "%s\\s *"
                                                   (regexp-quote sym))))))
                  pairs)))))

(defun gauche-paren-type-at-after-reader-macro-p (car)
  (gauche-paren-type-gen-reader-macro-condition car
                                                ("'" "quote")
                                                ("`" "quasiquote")
                                                ("," "unquote")
                                                (",@" "unquote-splicing")
                                                ("#?=" "debug-print")))

(defun gauche-paren-type-in-char-or-string-or-comment-p ()
  (or (eq (char-before) ?\\)
      (save-excursion
        (let ((point (point)))
          (beginning-of-defun)
          (let ((state (parse-partial-sexp (point) point)))
            (or (nth 3 state)
                (nth 4 state)))))))

(defun gauche-paren-type-highlight (start end)
  "highlight parentheses in the region specified by the arguments"
  (save-excursion
    (gauche-paren-type-read-info)
    (goto-char start)
    (beginning-of-defun)
    (let ((info gauche-paren-type-info))
      (while (and info (< (point) end))
        (let ((line (line-number-at-pos)))
          (while (and info
                      (< (gauche-paren-type--info-line (car info)) line))
            (pop info)))
        (let ((bound (save-excursion
                       (forward-line)
                       (point)))
              (p0 (point)))
          (if (or (null info)
                  (not (search-forward-regexp "[[(]" bound t)))
              (goto-char bound)
            (let* ((ed (match-end 0))
                   (inf (car info))
                   (car (gauche-paren-type-info-car inf))
                   (typ (gauche-paren-type-info-type inf)))
              (goto-char (match-beginning 0))
              (cond
               ((gauche-paren-type-in-char-or-string-or-comment-p)
                (remove-text-properties (1- (point))
                                        (point)
                                        '(font-lock-face))
                (goto-char ed))
               ((and (= (line-number-at-pos)
                        (gauche-paren-type--info-line inf))
                     (looking-at (format "[[(]\\s *%s\\S_" (regexp-quote car))))
                (gauche-paren-type-highlight-current-list
                 (case typ
                   ((procedure)
                    'gauche-paren-type-proc-paren-face)
                   ((macro)
                    'gauche-paren-type-macro-paren-face)))
                (pop info)
                (goto-char (match-end 0)))
               ((gauche-paren-type-at-after-reader-macro-p car)
                (pop info)
                (gauche-paren-type-highlight-current-list
                 'gauche-paren-type-other-paren-face)
                (goto-char ed))
               (t
                (gauche-paren-type-highlight-current-list
                 'gauche-paren-type-other-paren-face)
                (goto-char ed)))))
          (unless (> (point) p0)
            (error "inifinite loop!")))))))

(define-minor-mode gauche-paren-type-mode
  "highlight parentheses based on their type (procedure call, macro call, or other)"
  :lighter "(P)"
  (cond (gauche-paren-type-mode
         (jit-lock-register #'gauche-paren-type-highlight))
        (t
         (jit-lock-unregister #'gauche-paren-type-highlight))))

(provide 'gauche-paren-type)
