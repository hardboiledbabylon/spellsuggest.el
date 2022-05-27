;; Copyright © 2019 by D. F. Hall <authorfunction@hardboiledbabylon.com>

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;; utility functions for suggesting spelling alternatives

;; assumes some kind of ispell -a compatibility

(defgroup spellsuggest nil
  "Faces for highlighting arg components of spelling suggestions buffer."
  :group 'spellsuggest)

(defface spellsuggest-arg-face
  '((t (:foreground "yellow")))
  "Face for `spellsuggest-arg-face`."
  :group 'spellsuggest)

(defvar spellsuggest-arg-face 'spellsuggest-arg-face)

(defvar spellsuggest-vertical-mode t)

;;(defvar spellsuggest-program "/usr/local/bin/hunspell")
;;(defvar spellsuggest-args '("-i" "UTF8"))
(defvar spellsuggest-program "/usr/local/bin/aspell")
(defvar spellsuggest-args '("pipe"))
;; has to account for adding space before each arg
(defvar spellsuggest-prog (concat spellsuggest-program " "
                                  (mapconcat 'identity
                                             spellsuggest-args " ")))

(defun spellsuggest-variables-update ()
  (setq spellsuggest-prog (concat spellsuggest-program " "
                                  (mapconcat 'identity
                                             spellsuggest-args " "))))

(defun spellsuggest--buf-gen-name (b)
  (concat "*--spell--" b "--*"))

(defun spellsuggest--popup-alternatives (buf words)
  (let ((temp-buffer-max-height (- (window-body-height) 1))
        (bufname (spellsuggest--buf-gen-name buf)))
    (with-current-buffer-window bufname
                                '((display-buffer-below-selected)
                                  (window-height . fit-window-to-buffer))
                                nil
                                (font-lock-mode)
                                (insert words)
                                (spellsuggest--buf-digitalize)
                                (spellsuggest--columnize-buf-words
                                 (window-body-width))
                                (spellsuggest--buf-colorize)
                                )))

(setq spellsuggest--rep-args-list
      '( ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9      
            ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
            ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
            ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
            ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
            ?! ?@ ?# ?$ ?% ?^ ?& ?* ?+ ?=))

(setq spellsuggest-syntax-table
      (let (( table (make-syntax-table text-mode-syntax-table)))
        (dolist (ob (append '(?\' ?\)) spellsuggest--rep-args-list) nil)
          (modify-syntax-entry ob "w" table))
        table ))

(defun spellsuggest-and-replace ()
  (interactive)
  (if buffer-read-only
      (message "Buffer is read-only")
    (spellsuggest--word)))

;; if popup used a new window prev-buffers will return nil
(defun spellsuggest--delete-buf-window (buf)
  (when buf
    (let ((wins (get-buffer-window-list buf)))
      (when wins
        (dolist (win wins)
          (if (window-prev-buffers win)
              (switch-to-prev-buffer win)
            (delete-window win)))))
    (kill-buffer buf)))

;;                  (cond ((= k #x7)  C-g
(defun spellsuggest--query-replace (lst buf)
  (when lst
    (let* ((lsbuf (spellsuggest--buf-gen-name buf))
           (prompt "which:")
           (asking t))
      (while asking
        (let ((k (read-key prompt)))
          (cond ((= k #x20) ;; space
                 (setq asking nil))
                ((with-current-buffer buf
                   (spellsuggest--do-replace k lst))
                 (setq asking nil))
                (t
                 (setq prompt "non-extant key, again, which:"))))))))

(defun spellsuggest--map-key (k lst)
  (if (and k lst)
      (catch 'ret
        (let ((off 0))
          (dolist (i spellsuggest--rep-args-list)
            (when (= k i)          
              (throw 'ret (nth off lst)))
            (setq off (1+ off))))
        nil)
    nil))

;; thing at point has already been validated before
;; this is called so don't check again
(defun spellsuggest--do-replace (k l)
  (if (and k l)
      (let* ((b (bounds-of-thing-at-point 'word))
             (beg (car b))
             (end (cdr b))
             (rep (spellsuggest--map-key k l)))
        (if rep
            (progn
              (goto-char beg)
              (delete-region beg end)
              (insert rep)
              t)
          nil))
    nil))

;; There's a joke here if you know that originally
;; it was conceived that it would only label the first
;; 10 options. The naming scheme remains from that, even
;; though the prior assumptions were discarded.
(defun spellsuggest--buf-digitalize ()
  (goto-char (point-min))
  (dolist (d spellsuggest--rep-args-list)
    (unless (eobp)
      (insert (concat (string d) ")"))
      (skip-chars-forward "^\n")
      (skip-chars-forward "\n"))))

(defun spellsuggest--buf-colorize ()
  (save-match-data
    (goto-char (point-min))
    (while (re-search-forward "[a-zA-Z0-9!@#$%^&+=])" nil t 1)
      (set-text-properties (match-beginning 0) (match-end 0)
                           '(face spellsuggest-arg-face)))))

(defun min--len (l)
  (setq min nil)
  (if l
      (dolist (w l)
        (let ((len (length w)))
          (if min
              (when (< (length w) min)
                (setq min len))
            (setq min len)))))
  min)

(defun max--len (l)
  (setq max nil)
  (if l
      (dolist (w l)
        (let ((len (length w)))
          (if max
              (when (> len max)
                (setq max len))
            (setq max len)))))
  max)

(defun insert--vertical-list-cols (lst w h)
  (when (and lst (> w 0) (> h 0))
      (let ((w (if (> w h) h w))
            (h (if (> w h) w h))
            (c 0) (r 0))
          (dolist (word lst)
            (if (= c 0)
                (progn
                (insert word)
                (newline))
              (end-of-line)
              (insert " ")
              (insert word)
              (forward-line) )
            (setq r (1+ r)) 
            (when (>= r h)
              (setq r 0)
              (goto-char (point-min))
              (setq c (1+ c)))))))
          
(defun insert--horizontal-list-cols (lst w h)
  ;; if current-column + length ! fit -> newline
  (when (and lst (> w 0) (> h 0))
    (goto-char (point-min))

    (let ((c 0))
      (dolist (word lst)
        (insert word)
        (setq c (1+ c))
        (if (>= c w)
            (progn
              (newline)
              (setq c 0))
          (insert " "))))))

(defun pad--list (lst max)
  (setq out '())
  (if (and lst max)
      (dolist (w lst out)
        (let ((len (length w)))
          (if (< len max)
              (setq out (append out
                                (list
                                 (concat w (make-string (- max len) #x0020)))))
            (setq out (append out (list w))))))))

;; echo word should get its own line
(defun spellsuggest--columnize-buf-words (width)
  (let* ((words (buffer-substring-no-properties (point-min) (point-max)))
         (wlist (split-string words "\n" t nil))
         (wmin (min--len wlist))
         (wmax (max--len wlist))
         (maxcols (- (/ width (+ wmax 1)) 1))
         (maxrows (+ (/ (length wlist) maxcols)
                     (if (> (% (length wlist) maxcols) 0)
                         1 0))))
    
    (delete-region (point-min) (point-max))
    (let ((wpad (pad--list wlist wmax)))
      (if spellsuggest-vertical-mode
          (insert--vertical-list-cols wpad maxcols maxrows)
        (insert--horizontal-list-cols wpad maxcols maxrows)))

    (goto-char (point-min))
    (while (re-search-forward " +$" nil t 1)
	              (replace-match "")) ))

;; shell-command-to-string isn't really the best way to do this
;; but until emacs has some notion of synchronous processes
;; that's different than the current model, and since this
;; function expressly doesn't matter if it waits, because someone
;; will be waiting for the output, it's the best option at the
;; moment.

(defun spellsuggest--word ()
  ;; new window will pop (usually) above or below
  ;; current one (this should always be used interactively)
  ;; so the width of active window will be (hopefully)
  ;; width of popup window
  (let ((w (thing-at-point 'word t))
        (sbuf (buffer-name (current-buffer)))
        (word-list nil))

    (if w
        (with-temp-buffer
          (insert (shell-command-to-string
                   (concat "echo " (shell-quote-argument w)
                           " | " spellsuggest-prog)))
          (goto-char (point-min))
          (delete-region (line-beginning-position) (1+ (line-end-position)))
          (let (( c (char-after (point-min))))
            (cond ( (= c ?&)
                    (save-match-data
                      (goto-char (point-max))
                      (if (< (skip-chars-backward "\n") 0)
                          (delete-region (point) (point-max)))
                      (goto-char (point-min))
                      (when (search-forward ": " nil t 1)
                        (delete-region (point-min) (point))
                        ;; comma divides words (followed by space)
                        ;;& monopolistically 6 0: monopolistic ally, monopolistic-ally, monopolistic, moralistically, nationalistically, opportunistically
                        (when (> (skip-chars-forward "^\n") 0)
                          (delete-region (point) (point-max)))
                        (goto-char (point-min))
			(setq word-list (split-string
                                         (buffer-substring-no-properties
                                          (point-min) (point-max)) "," t
                                          " +"))
                        (while (search-forward ", " nil t 1)
	                  (replace-match "\n")) ))

                    (spellsuggest--popup-alternatives
                     sbuf
                     (buffer-substring
                      (point-min)
                      (point-max)))
                    (spellsuggest--query-replace word-list sbuf)
                    (spellsuggest--delete-buf-window
                     (spellsuggest--buf-gen-name sbuf))
                    (setq out t))
                  ( (= c ?#)
                    (message "no potential alternatives found"))
                  ( t
                    (message "word located in dictionary")))))
      (message "No word at point"))))

(provide 'spellsuggest)
