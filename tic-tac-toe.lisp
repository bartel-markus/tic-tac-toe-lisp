;;;; tic-tac-toe.lisp

(in-package #:tic-tac-toe)

;;; Define the board as a 3x3 2D array
(defparameter *board* (make-array (list 3 3) :initial-element " "))

;;; Define the player's symbol
(defparameter *player* "X")

;; Define winning arrangements of indices with same symbol
(defparameter *winning-constellations*
  `(((0 0) (0 1) (0 2))                 ;indices of rows
    ((1 0) (1 1) (1 2))
    ((2 0) (2 1) (2 2))
    ((0 0) (1 0) (2 0))                 ;indices of columns
    ((0 1) (1 1) (2 1))
    ((0 2) (1 2) (2 2))
    ((0 0) (1 1) (2 2))                 ;indices of diagonals
    ((0 2) (1 1) (2 0))))

;;; Define a function to print the board
(defun print-board ()
  (princ "┌───┬───┬───┐")
  (terpri)
  (dotimes (i 3)
    (princ "│ ")
    (dotimes (j 3)
      (princ (aref *board* i j))
      (princ (if (= j 2) " │" " │ ")))
    (terpri)
    (if (= i 2)
        (princ "└───┴───┴───┘")
        (princ "├───┼───┼───┤"))
    (terpri)))

;;; Define a function to change the player's turn
(defun change-player ()
  (setf *player* (if (string= *player* "X") "O" "X")))

(defun all-equal-strings-p (lst)
  (every #'(lambda (s) (string= (car lst) s)) (cdr lst)))

(defun sequence-of-winning-fields-p (indices)
  (let ((field-content-lst (mapcar #'(lambda (ind)
                                       (aref *board* (car ind) (cadr ind)))
                                   indices)))
    (and (not (member " " field-content-lst))
         (all-equal-strings-p field-content-lst))))

(defun game-over ()
  (some #'sequence-of-winning-fields-p *winning-constellations*))

;;; Define a function to handle the game loop
(defun play-game ()
  (loop
    (print-board)
    (format t "Player ~A, please enter the row and column (e.g. 0 0): " *player*)
    (let ((row (read)) (column (read)))
      (if (string= " " (aref *board* row column))
          (progn
            (setf (aref *board* row column) *player*)
            (when (game-over)
              (print-board)
              (return (format t "Player ~A wins!~%" *player*)))
            (change-player))
          (format t "The field is already occupied! Try again, Player ~A!~%" *player*)))))

;;; Start the game
;; (play-game)
