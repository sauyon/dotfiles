;;; 2048.el --- play 2048 in Emacs

;; Copyright 2014 Zachary Kanfer

;; Author: Zachary Kanfer <zkanfer@gmail.com>
;; Version: 20140618.2327
;; X-Original-Version: 2014.03.27
;; URL: https://bitbucket.org/zck/2048.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-derived-mode 2048-mode special-mode "2048-mode"
  (define-key 2048-mode-map (kbd "p") '2048-up)
  (define-key 2048-mode-map (kbd "C-p") '2048-up)
  (define-key 2048-mode-map (kbd "<up>") '2048-up)
  (define-key 2048-mode-map (kbd "n") '2048-down)
  (define-key 2048-mode-map (kbd "C-n") '2048-down)
  (define-key 2048-mode-map (kbd "<down>") '2048-down)
  (define-key 2048-mode-map (kbd "b") '2048-left)
  (define-key 2048-mode-map (kbd "C-b") '2048-left)
  (define-key 2048-mode-map (kbd "<left>") '2048-left)
  (define-key 2048-mode-map (kbd "f") '2048-right)
  (define-key 2048-mode-map (kbd "C-f") '2048-right)
  (define-key 2048-mode-map (kbd "<right>") '2048-right))

(defun 2048-game () "Start playing 2048"
  (interactive)
  (switch-to-buffer "2048")
  (2048-mode)
  (2048-init))

(defvar *2048-board* nil
  "The board itself. If a number is in the square, the number is stored. Otherwise, 0 is stored.
   You should access this with 2048-get-cell.")

(defvar *2048-combines-this-move* nil
  "This stores, for each cell in the board, whether the number in it was generated this turn by two numbers combining.")

(defvar *2048-columns* 4
  "The width of the board. It could be customized, if you wanted to make the game very very hard, or very very easy.")

(defvar *2048-rows* 4
  "The height of the board. It could be customized, if you wanted to make the game very very tall, or very very short.")

(defvar *2048-possible-values-to-insert* (cons 4 (make-list 9 2))
  "When a new element is inserted into the board, randomly choose a number from this sequence.")

(defvar *2048-victory-value* 2048
  "When this number is reached, the user wins! Yay!")

(defvar *2048-debug* nil
  "when 't, print debugging information.")

(defconst *2048-numbers* '(0 2 4 8 16 32 64 128 256 512 1024 2048))

(defvar *2048-score* nil
  "Current score in the game. Incremented by the sum of 2 equal tiles when they are collapsed")

(defvar *2048-hi-tile* nil
  "Current highest-number tile")

(defvar *2048-history* nil
  "Score history in this Emacs session. Each element is (SCORE HI-TILE TIME)")

;; If you want to pick some better colors, see (2048-test-tiles)
(defface 2048-face-2    '((t . (:background "khaki" :foreground "black"))) "Face for the tile 2" :group '2048-faces)
(defface 2048-face-4    '((t . (:background "burlywood" :foreground "black"))) "Face for the tile 4" :group '2048-faces)
(defface 2048-face-8    '((t . (:background "orange3" :foreground "black"))) "Face for the tile 8" :group '2048-faces)
(defface 2048-face-16   '((t . (:background "orange" :foreground "black"))) "Face for the tile 16" :group '2048-faces)
(defface 2048-face-32   '((t . (:background "orange red" :foreground "black"))) "Face for the tile 32" :group '2048-faces)
(defface 2048-face-64   '((t . (:background "firebrick" :foreground "white"))) "Face for the tile 64" :group '2048-faces)
(defface 2048-face-128  '((t . (:background "dark red" :foreground "white"))) "Face for the tile 128" :group '2048-faces)
(defface 2048-face-256  '((t . (:background "dark magenta" :foreground "white"))) "Face for the tile 256" :group '2048-faces)
(defface 2048-face-512  '((t . (:background "magenta" :foreground "black"))) "Face for the tile 512" :group '2048-faces)
(defface 2048-face-1024 '((t . (:background "gold" :foreground "black"))) "Face for the tile 1024" :group '2048-faces)
(defface 2048-face-2048 '((t . (:background "yellow" :foreground "black"))) "Face for the tile 2048" :group '2048-faces)

(defun 2048-empty (n)
  "Return symbol of the variable holding empty space for number N"
  (intern (concat "2048-empty-" (int-to-string n))))

(defun 2048-tile (n)
  "Return symbol of the variable holding tile for number N"
  (intern (concat "2048-tile-" (int-to-string n))))

(defmacro 2048-for (var init end &rest body)
  "Helper function. executes 'body repeatedly, with 'var assigned values starting at 'init, and ending at 'end, increasing by one each iteration."
  `(let ((,var ,init)
	 (end-val ,end))
     (while (<= ,var end-val)
       ,@body
       (setq ,var (1+ ,var)))))

(defmacro 2048-for-down (var init end &rest body)
  "Helper function, executes 'body repeatedly, with 'var assigned values starting at 'init, and ending at 'end, decreasing by one each iteration."
  `(let ((,var ,init)
	 (end-val ,end))
     (while (>= ,var end-val)
       ,@body
       (setq ,var (1- ,var)))))

(defmacro 2048-game-move (&rest body)
  `(progn (setq *2048-combines-this-move* (make-vector (* *2048-columns* *2048-rows*)
                                               nil))

          ,@body
          (2048-print-board)
          (2048-check-game-end)))

(defmacro 2048-debug (&rest body)
  "If *2048-debug* is 't, log ,@body as a string to the buffer named '2048-debug'"
  `(when *2048-debug*
     (print (concat ,@body)
	    (get-buffer-create "2048-debug"))))

(defun 2048-init-tiles ()
  "Init each variable 2048-empty-N and 2048-tile-N with appropriate string and face"
  (mapc #'(lambda (num)
            (set (2048-empty num) (format "%7s" " "))
            ;; if constant then all faces are applied to this one constant. (Symptom: all background is yellow)
            ;; The bytecompiler is smart enough to see that (concat...) is a constant, but not (format...) ;-)
            (set (2048-tile num) (format "%5s  " (2048-num-to-printable num)))
            (when (> num 0)
              (let ((face (intern (concat "2048-face-" (int-to-string num)))))
                (put-text-property 0 7 'font-lock-face face (symbol-value (2048-empty num)))
                (put-text-property 0 7 'font-lock-face face (symbol-value (2048-tile num))))))
        *2048-numbers*))

(defun 2048-test-tiles ()
  "Test out the tile colors"
  (interactive)
  (let ((*2048-board*
         (vconcat *2048-numbers*
                  (make-vector (- (* *2048-columns* *2048-rows*)
                                  (length *2048-numbers*))
                               0)))
        (*2048-score* 123456)
        (*2048-history* '((123  512 "2014-06-18 12:34:56")
                          (456 1024 "2014-06-18 12:45:00"))))
    (switch-to-buffer "2048-test")
    (2048-init-tiles)
    (2048-mode)
    (2048-print-board)))

(defun 2048-init ()
  "Begin a game of 2048."
  (setq *2048-board* (make-vector (* *2048-columns* *2048-rows*)
                                  0))
  (setq *2048-combines-this-move* (make-vector (* *2048-columns* *2048-rows*)
                                               nil))
  (setq *2048-score* 0
        *2048-hi-tile* 2)
  (2048-insert-random-cell)
  (2048-insert-random-cell)
  (2048-init-tiles)
  (2048-print-board)
  (message "Good luck!"))

(defun 2048-get-cell (row col)
  "Gets the value in (row, col)."
  (elt *2048-board*
       (+ (* row *2048-columns*)
          col)))

(defun 2048-set-cell (row column val)
  "Sets the value in (row, column)."
  (when (< *2048-hi-tile* val)
    (setq *2048-hi-tile* val))
  (aset *2048-board*
        (+ (* row *2048-columns*)
           column)
        val))

(defun 2048-num-to-printable (num)
  "If you pass in 0, returns an empty string. Otherwise, returns the number as a string."
  (if (eq num 0)
      ""
    (format "%d" num)))

(defun 2048-was-combined-this-turn (row column)
  "Returns whether the number in it was generated this turn by two numbers combining."
  (elt *2048-combines-this-move*
       (+ (* row *2048-columns*)
          column)))

(defun 2048-set-was-combined-this-turn (row column)
  "Returns whether the number in it was generated this turn by two numbers combining."
  (2048-debug (format "setting (%d, %d) as combined this turn." row column))
  (aset *2048-combines-this-move*
        (+ (* row *2048-columns*)
           column)
        t))

(defun 2048-insert-random-cell ()
  "Picks a number randomly, and inserts it into a random cell."
  (let ((number-to-insert (elt *2048-possible-values-to-insert*
                               (random (length *2048-possible-values-to-insert*))))
        (row (random *2048-rows*))
        (column (random *2048-columns*)))
    (while (not (eq (2048-get-cell row column)
                    0))
      (setq row (random *2048-rows*))
      (setq column (random *2048-columns*)))
    (2048-set-cell row column number-to-insert)))

(defun 2048-check-game-end ()
  "Checks whether the game has either been won or lost. If so, it handles notifying and restarting."
  (cond ((2048-game-was-won)
         (2048-add-new-history-item *2048-score* *2048-hi-tile* (current-time))
         (when (y-or-n-p "Yay! You beat the game! Want to push your luck?")
           (2048-init)))
        ((2048-game-was-lost)
         (2048-add-new-history-item *2048-score* *2048-hi-tile* (current-time))
         (when (y-or-n-p "Aw, too bad. You lost. Want to play again?")
           (2048-init)))))

(defun 2048-add-new-history-item (score hi-tile time)
  "Generates and adds a new history item to the score list, keeping the list in order by score."
  (setq *2048-history* (sort* (cons (list *2048-score* *2048-hi-tile*
                                          (format-time-string "%Y-%m-%d %H:%M:%S"
                                                              (or time (current-time))))
                                    *2048-history*)
                              '>
                              :key 'car)))

(defun 2048-game-was-won ()
  "Returns t if the game was won, nil otherwise."
  (let ((game-was-won nil))
    (2048-for row 0 (1- *2048-rows*)
              (2048-for column 0 (1- *2048-columns*)
                        (when (eq (2048-get-cell row column)
                                  *2048-victory-value*)
                          (setq game-was-won t))))
    game-was-won))

(defun 2048-game-was-lost ()
  "Returns t if the game was lost, nil otherwise."
  (let ((game-was-lost t))
    (2048-for row 0 (1- *2048-rows*)
              (2048-for column 0 (1- *2048-columns*)
                        (when (eq (2048-get-cell row column)
                                  0)
                          (setq game-was-lost nil))))

    ;; For each square, if that square has one below it that's the same,
    ;; the game's not over.
    (2048-for row 0 (- *2048-rows* 2)
              (2048-for column 0 (1- *2048-columns*)
                        (when (eq (2048-get-cell row column)
                                      (2048-get-cell (1+ row) column))
                          (setq game-was-lost nil))))

    ;; For each square, if that square has one to its right that's the same,
    ;; the game's not over.
    (2048-for column 0 (- *2048-columns* 2)
              (2048-for row 0 (1- *2048-rows*)
                        (when (eq (2048-get-cell row column)
                                  (2048-get-cell row (1+ column)))
                          (setq game-was-lost nil))))
    game-was-lost))

(defun 2048-print-board ()
  "Wipes the entire field, and prints the board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *2048-rows*)

      ;;print the separator line on top of, and between cells
      (dotimes (col *2048-columns*)
        (insert "+-------"))
      (insert "+")
      (insert "\n")

      ;;print the empty line above numbers
      (dotimes (col *2048-columns*)
        (insert "|")
        (let ((current-value (2048-get-cell row col)))
          (insert (symbol-value (2048-empty current-value)))))
      (insert "|")
      (insert "\n")

      ;; print the number tiles
      (dotimes (col *2048-columns*)
        (insert "|")
        (let ((current-value (2048-get-cell row col)))
          (insert (symbol-value (2048-tile current-value)))))
      (insert "|")
      (insert "\n")

      ;;print the empty line below numbers
      (dotimes (col *2048-columns*)
        (insert "|")
        (let ((current-value (2048-get-cell row col)))
          (insert (symbol-value (2048-empty current-value)))))
      (insert "|")
      (insert "\n"))

    ;;print the separator line on the bottom of the last row.
    (dotimes (col *2048-columns*)
      (insert "+-------"))
    (insert "+\n")

    ;; print score and history
    (insert (format "%8d\n" *2048-score*)
            (format "%8s %7s  %s\n" "Score" "Hi-Tile" "Date       Time"))
    (mapc #'(lambda (x)
              (insert (format "%8d %7d  %s\n"
                              (first x) (second x) (third x))))
          *2048-history*)))

(defun 2048-move (from-row from-column delta-row delta-column)
  "Tries to move the number in (from-row, from-column) to (to-row, to-column).
   This succeeds when (to-row, to-column) either is 0,
   or is the same value as (from-row, from-column).
   If (to-row, to-column) is zero, cascade and try to move further.
   Returns t if we were able to move; otherwise nil."
  (let ((to-row (+ from-row delta-row))
        (to-column (+ from-column delta-column)))
    (when (in-bounds to-row to-column)
      (2048-debug (format "moving the cell (%d, %d) by (%d, %d) to (%d, %d)" from-row from-column delta-row delta-column to-row to-column))
      (let ((from-val (2048-get-cell from-row from-column))
            (to-val (2048-get-cell to-row to-column)))
        (cond ((eq from-val to-val)
               (unless (or (eq from-val 0)
                           (2048-was-combined-this-turn to-row to-column))
                 (2048-debug (format "combining (%d, %d) into (%d, %d)" from-row from-column to-row to-column))
                 (2048-set-cell to-row to-column (* from-val 2))
                 (setq *2048-score* (+ *2048-score* (* from-val 2)))
                 (2048-set-cell from-row from-column 0)
                 (2048-set-was-combined-this-turn to-row to-column)))
              ((eq to-val 0)
               (2048-set-cell to-row to-column from-val)
               (2048-set-cell from-row from-column 0)
               (2048-move to-row to-column delta-row delta-column)
               t)
              (t nil)))))) ;;ugh, need to pass out whether something was combined, and pass that to the _next_ call to 2048-move. We see bugs on rows like 4 0 4 0.

(defun in-bounds (row column)
  (and (>= row 0)
       (>= column 0)
       (< row *2048-rows*)
       (< column *2048-columns*)))


(defun 2048-up ()
  "Shifts the board up"
  (interactive)
  (2048-game-move
   (setq *2048-combines-this-move* (make-vector (* *2048-columns* *2048-rows*)
                                                nil))
   (let ((has-moved nil))
     (2048-for col 0 (1- *2048-columns*)
               (2048-for row 1 (1- *2048-rows*)
                         (setq has-moved (or (2048-move row col -1 0)
                                             has-moved))))
     (when has-moved
       (2048-insert-random-cell)))))

(defun 2048-down ()
  "Shifts the board down"
  (interactive)
  (2048-game-move
   (setq *2048-combines-this-move* (make-vector (* *2048-columns* *2048-rows*)
                                                nil))
   (let ((has-moved nil))
     (2048-for col 0 (1- *2048-columns*)
               (2048-for-down row (- *2048-rows* 2) 0
                              (setq has-moved (or (2048-move row col 1 0)
                                                  has-moved))))
     (when has-moved
       (2048-insert-random-cell)))))

(defun 2048-left ()
  "Shifts the board left."
  (interactive)
  (2048-game-move
   (let ((has-moved nil))
     (2048-for row 0 (1- *2048-rows*)
               (2048-for col 1 (1- *2048-columns*)
                         (setq has-moved (or (2048-move row col 0 -1)
                                             has-moved))))
     (when has-moved
       (2048-insert-random-cell)))))

(defun 2048-right ()
  "Shifts the board right."
  (interactive)
  (2048-game-move
   (let ((has-moved nil))
     (2048-for row 0 (1- *2048-rows*)
               (2048-for-down col (- *2048-columns* 2) 0
                              (setq has-moved (or (2048-move row col 0 1)
                                                  has-moved))))
     (when has-moved
       (2048-insert-random-cell)))))

(provide '2048-game)
;;; 2048.el ends here

;;; 2048-game.el ends here
