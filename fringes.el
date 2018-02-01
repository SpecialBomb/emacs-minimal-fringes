;; SYMBOLS DESIGNED BY: Quentin Jankosky -- https://github.com/SpecialBomb/emacs-minimal-fringes
;; BITMAPPER FUNCTION BY:  Nikolaj Schumacher -- https://github.com/nschum/fringe-helper.el
;;

(fringe-mode (quote (8 . 8)) nil (fringe))

(defun fringe-helper-convert (&rest strings)
"Convert STRINGS into a vector usable for `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap.
Periods (.) are background-colored pixel; Xs are foreground-colored. The
fringe bitmap always is aligned to the right. If the fringe has half
width, only the left 4 pixels of an 8 pixel bitmap will be shown.
For example, the following code defines a diagonal line.
\(fringe-helper-convert
\"XX......\"
\"..XX....\"
\"....XX..\"
\"......XX\"\)"
  (unless (cdr strings)
  ;; only one string, probably with newlines
    (setq strings (split-string (car strings) "\n")))
  (apply 'vector
    (mapcar
      (lambda (str)
        (let ((num 0))
          (dolist (c (string-to-list str))
            (setq num (+ (* num 2) (if (eq c ?.) 0 1))))
          num))
      strings)))

(define-fringe-bitmap 'right-arrow (fringe-helper-convert
  "X......."
  "XX......"
  "XXX....."
  "XXXX...."
  "XXXXX..."
  "XXXXXX.."
  "XXXXXXX."
  "XXXXXX.."
  "XXXXX..."
  "XXXX...."
  "XXX....."
  "XX......"
  "X.......") nil nil 'center)

(define-fringe-bitmap 'left-arrow (fringe-helper-convert
  "......X"
  ".....XX"
  "....XXX"
  "...XXXX"
  "..XXXXX"
  ".XXXXXX"
  "XXXXXXX"
  ".XXXXXX"
  "..XXXXX"
  "...XXXX"
  "....XXX"
  ".....XX"
  "......X") nil nil 'center)

(define-fringe-bitmap 'left-curly-arrow (fringe-helper-convert
  "......."
  "......."
  ".....X."
  "....X.."
  "...X..."
  "..X...."
  ".X...X."
  "..X..X."
  "...X.X."
  "....XX."
  ".XXXXX."
  "......."
  ".......") nil nil 'center)

(define-fringe-bitmap 'right-curly-arrow (fringe-helper-convert
  "........"
  "........"
  ".X......"
  "..X....."
  "...X...."
  "....X..."
  ".X...X.."
  ".X..X..."
  ".X.X...."
  ".XX....."
  ".XXXXX.."
  "........"
  "........") nil nil 'center)

(define-fringe-bitmap 'right-triangle (fringe-helper-convert
  ".......X"
  "......X."
  ".....XXX"
  "....X..."
  "...XXXXX"
  "..X....."
  ".XXXXXXX"
  "..X....."
  "...XXXXX"
  "....X..."
  ".....XXX"
  "......X."
  ".......X") nil nil 'center)

(define-fringe-bitmap 'right-triangle (fringe-helper-convert
  "X......."
  ".X......"
  "XXX....."
  "...X...."
  "XXXXX..."
  ".....X.."
  "XXXXXXX."
  ".....X.."
  "XXXXX..."
  "...X...."
  "XXX....."
  ".X......"
  "X.......") nil nil 'center)

