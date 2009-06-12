;;; Ollibot editing mode

;; Ollibot's text mode is currently very simple; it only sets the input method
(define-derived-mode ollibot-mode nil "Ollibot"
 "Major mode for ollibot files. Currently does nothing but force .olf files
to be opened with the correct encoding."
 (set-input-method "Ollibot"))

;; Automatically load ollibot-mode on files
(add-to-list 'auto-mode-alist '("\\.olf$" . ollibot-mode))

;; Make the universal change to presume that all subprocess communication UTF-8
;; This could be problematic on, for instance, unix systems, so it would be
;; better if we worked out subprocess communication and used that instead.
(setq-default default-process-coding-system (cons 'mule-utf-8 'mule-utf-8))

;; Set up the quail input-method for Ollibot
(require 'quail)
(require 'cl)

(quail-define-package "Ollibot" "UTF-8" "•" t
  "Ollibot input method.
The purpose of this input method is to edit .olf programs."
 
 '(("\t" . quail-completion))
 t t nil nil nil nil nil nil nil t)

(quail-define-rules

 ;; Binders
 ("\\\\" ?λ)
 ("\\lambda" ?λ)
 ("\\ex" ?∃)
 ("\\exists" ?∃)

 ;; Connectives
 ("\\!" ?¡)
 ("!`" ?¡)
 ("\\all" ?∀)
 ("\\forall" ?∀)
 ("\\fuse" ?•)
 ("\\bullet" ?•)
 ("\\esuf" ?◦)
 ("\\circ" ?◦)

 ;; Mathematical superscripts and subscripts
 ("^(" ?⁽)
 ("^)" ?⁾)
 ("^+" ?⁺)
 ("^-" ?⁻)
 ("^0" ?⁰)
 ("^1" ?¹)
 ("^2" ?²)
 ("^3" ?³)
 ("^4" ?⁴)
 ("^5" ?⁵)
 ("^6" ?⁶)
 ("^7" ?⁷)
 ("^8" ?⁸)
 ("^9" ?⁹)
 ("^=" ?⁼)
 ("_(" ?₍)
 ("_)" ?₎)
 ("_+" ?₊)
 ("_-" ?₋)
 ("_0" ?₀)
 ("_1" ?₁)
 ("_2" ?₂)
 ("_3" ?₃)
 ("_4" ?₄)
 ("_5" ?₅)
 ("_6" ?₆)
 ("_7" ?₇)
 ("_8" ?₈)
 ("_9" ?₉)
 ("_=" ?₌)
)
