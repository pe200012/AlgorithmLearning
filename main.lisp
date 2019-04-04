
(defmacro decr (x) `(- ,x 1))
(defmacro incr (x) `(+ ,x 1))

(defun neg (f)
  (lambda (&rest args) (not (apply f args))))

(defun swap (arr a b)
  (let ((old-a (svref arr a)))
    (progn
      (setf (svref arr a) (svref arr b))
      (setf (svref arr b) old-a)
      arr)))

(defun insert-sort (ls test)
  (labels ((myinsert (head tail x)
             (if (null tail)
                 (reverse (cons x head))
                 (if (funcall test x (car tail))
                     (append (reverse head) (cons x tail))
                     (myinsert
                       (cons (car tail) head)
                       (cdr tail)
                       x))))
           (helper (sorted rest)
             (if (null rest)
                 sorted
                 (helper
                       (myinsert
                         '()
                          sorted
                          (car rest))
                       (cdr rest)))))
    (helper '() ls)))


(defun partition (arr test left right)
  (let ((pivot left))
    (do ((iter left (+ iter 1)))
        ((> iter (decr right))
         (progn
           (swap arr pivot right)
           pivot))
      (if (funcall test (svref arr iter) (svref arr right))
          (progn
            (swap arr pivot iter)
            (setf pivot (incr pivot)))))))

(defun quick-sort (arr test &optional left right)
  (let ((left (if (null left) 0 left))
        (right (if (null right) (decr (array-total-size arr)) right)))
    (if (>= left right)
        arr
        (let ((pivot (partition arr test left right)))
          (progn
            (quick-sort arr test left (decr pivot))
            (quick-sort arr test (incr pivot) right))))))

