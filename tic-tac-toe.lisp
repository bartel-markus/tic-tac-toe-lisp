;;;; tic-tac-toe.lisp

(in-package #:tic-tac-toe)

;;; Define the board as a 3x3 2D array
(defparameter *board* (make-array (list 3 3) :initial-element " "))

;;; Define the player's symbol
(defparameter *player* "X")

;;; Define winning arrangements of indices with same symbol
(defparameter *winning-constellations*
  `(((1 1) (1 2) (1 3))                 ;indices of rows
    ((2 1) (2 2) (2 3))
    ((3 1) (3 2) (3 3))
    ((1 1) (2 1) (3 1))                 ;indices of columns
    ((1 2) (2 2) (3 2))
    ((1 3) (2 3) (3 3))
    ((1 1) (2 2) (3 3))                 ;indices of diagonals
    ((1 3) (2 2) (3 1))))

(defun aref-one-indexed (arr &rest subscripts)
  (apply #'aref arr (mapcar #'1- subscripts)))

(defun (setf aref-one-indexed) (new-value arr &rest subscripts)
  (setf (apply #'aref arr (mapcar #'1- subscripts)) new-value)
  new-value)

(defun listify-2D-array (arr)
  (destructuring-bind (n m) (array-dimensions arr)
    (loop for i below n collect
                        (loop for j below m collect
                                            (aref arr i j)))))

;;; Define a function to print the board
(defun print-board ()
  (format t "┌───┬───┬───┐~%~{│ ~{~a~^ │ ~} │~%~^├───┼───┼───┤~%~}└───┴───┴───┘~%"
          (listify-2D-array *board*)))

;;; Define a function to change the player's turn
(defun change-player ()
  (setf *player* (if (string= *player* "X") "O" "X")))

(defun all-equal-strings-p (lst)
  (every #'(lambda (s) (string= (car lst) s)) (cdr lst)))

(defun sequence-of-winning-fields-p (indices)
  (let ((field-content-lst (mapcar #'(lambda (ind)
                                       (aref-one-indexed *board* (car ind) (cadr ind)))
                                   indices)))
    (and (not (member " " field-content-lst))
         (all-equal-strings-p field-content-lst))))

(defun game-over ()
  (some #'sequence-of-winning-fields-p *winning-constellations*))

;;; Define a function to handle the game loop
(defun play-game ()
  (loop
    (terpri)
    (format t "Player ~A, please enter the row and column (e.g. 1 1): " *player*)
    (terpri)
    (print-board)
    (let ((row (read)) (column (read)))
      (if (string= " " (aref-one-indexed *board* row column))
          (progn
            (setf (aref-one-indexed *board* row column) *player*)
            (when (game-over)
              (print-board)
              (setf *board* (make-array (list 3 3) :initial-element " "))
              (return (format t "Player ~A wins!~%" *player*)))
            (change-player))
          (format t "The field is already occupied! Try again, Player ~A!~%" *player*)))))

;;; Start the game
;; (play-game)
