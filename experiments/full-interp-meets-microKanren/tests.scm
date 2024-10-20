(load "load.scm")

(test "pair?-0a"
  (run* (q)
    (evalo
     `(pair? ',q)
     #f))
  '(#f
    #t
    ()
    (_.0 (num _.0))
    (_.0 (=/= ((_.0 closure)) ((_.0 prim))) (sym _.0))))

(test "pair?-0b"
  (run* (q)
    (evalo
     `(pair? ',q)
     #t))
  '(((_.0 . _.1)
     (absento
      (closure _.0)
      (closure _.1)
      (prim _.0)
      (prim _.1)))))

(test "pair?-1"
  (run* (q)
    (evalo
     '(pair? 5)
     q))
  '(#f))

(test "pair?-2"
  (run* (q)
    (evalo
     '(pair? #f)
     q))
  '(#f))

(test "pair?-3"
  (run* (q)
    (evalo
     '(pair? #t)
     q))
  '(#f))

(test "pair?-4"
  (run* (q)
    (evalo
     '(pair? 'cat)
     q))
  '(#f))

(test "pair?-5"
  (run* (q)
    (evalo
     '(pair? '())
     q))
  '(#f))

(test "pair?-6"
  (run* (q)
    (evalo
     '(pair? cons)
     q))
  '(#f))

(test "pair?-7"
  (run* (q)
    (evalo
     '(pair? (lambda (x) x))
     q))
  '(#f))

(test "pair?-8"
  (run* (q)
    (evalo
     '(pair? (cons 3 4))
     q))
  '(#t))


(test "procedure?-0a"
  (run* (q)
    (evalo
     `(procedure? ',q)
     #f))
  '(#f
    #t
    ()
    (_.0 (num _.0))
    (_.0 (=/= ((_.0 closure)) ((_.0 prim))) (sym _.0))
    ((_.0 . _.1) (absento (closure _.0) (closure _.1) (prim _.0) (prim _.1)))))

(test "procedure?-0b"
  (run* (q)
    (evalo
     `(procedure? ',q)
     #t))
  '())

(test "procedure?-0c"
  (let ((procs
         (run 60 (q)
           (evalo
            `(procedure? ,q)
            #t))))
    (and (member '== procs)
         (member '((lambda _.0 _.1) (sym _.0)) procs)
         #t))
  #t)

(test "procedure?-1"
  (run* (q)
    (evalo
     '(procedure? 5)
     q))
  '(#f))

(test "procedure?-2"
  (run* (q)
    (evalo
     '(procedure? #f)
     q))
  '(#f))

(test "procedure?-3"
  (run* (q)
    (evalo
     '(procedure? #t)
     q))
  '(#f))

(test "procedure?-4"
  (run* (q)
    (evalo
     '(procedure? 'cat)
     q))
  '(#f))

(test "procedure?-5"
  (run* (q)
    (evalo
     '(procedure? '())
     q))
  '(#f))

(test "procedure?-6"
  (run* (q)
    (evalo
     '(procedure? (cons 3 4))
     q))
  '(#f))

(test "procedure?-7"
  (run* (q)
    (evalo
     '(procedure? cons)
     q))
  '(#t))

(test "procedure?-8"
  (run* (q)
    (evalo
     '(procedure? (lambda (x) x))
     q))
  '(#t))


(test "let-1"
  (run* (q)
    (evalo
     '(let ((z (cons 3 4)))
        (car z))
     q))
  '(3))

(test "let-2"
  (run* (q)
    (evalo
     '(let ((z (cons 3 4)))
        (let ((z (car z)))
          (list z z)))
     q))
  '((3 3)))

(test "let-3"
  (run* (q)
    (evalo
     '(let ((f (lambda (w) (cons w w))))
        (f (cons 3 4)))
     q))
  '(((3 . 4) . (3 . 4))))

(test "let-4"
  (run* (q)
    (evalo
     '(let ((f (lambda (w) (f w))))
        (f (cons 3 4)))
     q))
  '())


(test "map-1"
  (run* (q)
    (evalo
     '(map car '((a b) (c d e) (f)))
     q))
  '((a c f)))

(test "map-2"
  (run* (q)
    (evalo
     '(map (lambda (x) (null? x)) (list (list) #f '() (cons 3 4)))
     q))
  '((#t #f #t #f)))


(test "length-0"
  (run* (q)
    (evalo
     '(length '())
     q))
  '(()))

(test "length-1"
  (run* (q)
    (evalo
     '(length (cons 'a '()))
     q))
  '((())))

(test "length-2"
  (run* (q)
    (evalo
     '(length (list 'cat 'dog 'mouse))
     q))
  '((((())))))


(test "var?-1"
  (run* (q)
    (evalo
     '(var? '(lvar . ()))
     q))
  '(#t))

(test "var-1"
  (run* (q)
    (evalo
     '(var ())
     q))
  '())

(test "var-2"
  (run* (q)
    (evalo
     '(var '())
     q))
  '((lvar)))

(test "var?-2"
  (run* (q)
    (evalo
     '(var? (var '()))
     q))
  '(#t))

(test "var?-3"
  (run* (q)
    (evalo
     '(var? (var '()))
     q))
  '(#t))

(test "var=?-1"
  (run* (q)
    (evalo
     '(var=? (var '()) (var '()))
     q))
  '(#t))

(test "var=?-2"
  (run* (q)
    (evalo
     '(var=? (var '(())) (var '()))
     q))
  '(#f))

(test "var=?-3"
  (run* (q)
    (evalo
     '(var=? (var '(())) (var '(())))
     q))
  '(#t))


(test "evalo-walk-pair-1"
  (run* (q)
    (evalo
     `(walk
       '(lvar ())
       '(((lvar . (((())))) . 4)
         ((lvar . ((()))) . 3)
         ((lvar . (())) . ((lvar . ((()))) . (lvar . (((()))))))
         ((lvar . ()) . (lvar . (())))))
     q))
  '(((lvar . ((()))) . (lvar . (((())))))))

(test "evalo-walk*-pair-1"
  (run* (q)
    (evalo
     `(walk*
       '(lvar ())
       '(((lvar . (((())))) . 4)
         ((lvar . ((()))) . 3)
         ((lvar . (())) . ((lvar . ((()))) . (lvar . (((()))))))
         ((lvar . ()) . (lvar . (())))))
     q))
  '((3 . 4)))


(test "evalo-lambda/cons-1"
  (run* (q)
    (evalo
     '(let ((g (lambda (z) (cons z z))))
        (g (cons 3 4)))
     q))
  '(((3 . 4) . (3 . 4))))

(test "evalo-let/==/list-1"
  (car
   (car
    (car
     (run* (q)
       (evalo
        '(let ((g (== 3 4)))
           (list g))
        q)))))
  'closure)

(test "evalo-let/==/disj-1"
  (car
   (car
    (run* (q)
      (evalo
       '(let ((g1 (== 3 4)))
          (let ((g2 (== 5 5)))
            (disj g1 g2)))
       q))))
  'closure)

(test "evalo-let/==/conj-1"
  (car
   (car
    (run* (q)
      (evalo
       '(let ((g1 (== 3 4)))
          (let ((g2 (== 5 5)))
            (conj g1 g2)))
       q))))
  'closure)

(test "evalo-call/fresh/lambda/==-1"
  (car
   (car
    (run* (q)
      (evalo
       '(call/fresh (lambda (z) (== z 5)))
       q))))
  'closure)

(test "evalo-assp-1"
  (run 1 (p l q)
    (evalo
     `(assp ,p ,l)
     q))
  '(((_.0 '() #f) (num _.0))))

(test "evalo-assp-2"
  (run 2 (p l q)
    (evalo
     `(assp ,p ,l)
     q))
  '(((_.0 (quote ()) #f) (num _.0))
    (((quote _.0) (quote ()) #f) (absento (closure _.0) (prim _.0)))))

(test "evalo-assp-3"
  (run* (q)
    (evalo
     `(assp 'genny '())
     q))
  '(#f))

(test "evalo-assp-4"
  (run* (q)
    (evalo
     `(assp (lambda (y) (equal? y 'z)) '((z . 5)))
     q))
  '((z . 5)))

(test "evalo-assp-5"
  (run* (q)
    (evalo
     `(assp (lambda (y) (equal? y 'z)) '((a . 4) (b . 5) (c . 6) (b . 7)))
     q))
  '(#f))

(test "evalo-assp-6"
  (run* (q)
    (evalo
     `(assp (lambda (y) (equal? y 'b)) '((a . 4) (b . 5) (c . 6) (b . 7)))
     q))
  '((b . 5)))

#|
;; WEB -- about 20 seconds
(test "evalo-assp-7"
  (time
   (run 1 (p)
     (fresh (b)
       (== `(lambda (y) ,b) p))
     (evalo
      `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
      '(b . 5))))
  '((lambda (y) (equal? y 'b))))
|#

#|
;; WEB too slow--didn't come back after a couple of minutes
(test "evalo-assp-8"
  (run 1 (p)
    (evalo
     `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
     '(b . 5)))
  '())
|#

(test "evalo-fresh/==/assp-1"
  (time
   (run 1 (p)
     (fresh (b)
       (== `(lambda (y) (equal? . ,b)) p))
     (evalo
      `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
      '(b . 5))))
  '((lambda (y) (equal? 'b y))))

(test "evalo-fresh/==/assp-2"
  (time
   (run 1 (p)
     (fresh (x)
       (== `(lambda (y) (equal? y ,x)) p))
     (evalo
      `(assp ,p '((a . 4) (b . 5) (c . 6) (b . 7)))
      '(b . 5))))
  '((lambda (y) (equal? y 'b))))

(test "evalo-walk-1"
  (run* (q)
    (evalo
     `(walk '(lvar ()) '(((lvar (())) . 4) ((lvar ()) . 5)))
     q))
  '(5))

(test "evalo-walk-2"
  (run* (q)
    (evalo
     `(walk '(lvar ((()))) '(((lvar (())) . 4) ((lvar ()) . 5)))
     q))
  '((lvar ((())))))

(test "evalo-walk-3"
  (run* (q)
    (evalo
     `(walk 'cat '(((lvar (())) . 4) ((lvar ()) . 5)))
     q))
  '(cat))

(test "evalo-walk-4"
  (run* (q)
    (evalo
     `(walk '(lvar ()) '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(4))

(test "evalo-unify-1"
  (run* (q)
    (evalo
     `(unify '4 '4 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar (())) . 4) ((lvar ()) . (lvar (()))))))

(test "evalo-unify-2"
  (run* (q)
    (evalo
     `(unify '4 '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(#f))

(test "evalo-unify-3"
  (run* (q)
    (evalo
     `(unify '(lvar (())) '4 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar (())) . 4) ((lvar ()) . (lvar (()))))))

(test "evalo-unify-4"
  (run* (q)
    (evalo
     `(unify '(lvar (())) '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(#f))

(test "evalo-unify-5"
  (run* (q)
    (evalo
     `(unify '(lvar ()) '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '(#f))

(test "evalo-unify-6"
  (run* (q)
    (evalo
     `(unify '(lvar ()) '4 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar (())) . 4) ((lvar ()) lvar (())))))

(test "evalo-unify-7"
  (run* (q)
    (evalo
     `(unify '(lvar ()) '5 '())
     q))
  '((((lvar ()) . 5))))

(test "evalo-unify-8"
  (run* (q)
    (evalo
     `(unify '5 '(lvar ()) '())
     q))
  '((((lvar ()) . 5))))

(test "evalo-unify-9"
  (run* (q)
    (evalo
     `(unify '(lvar ((()))) '5 '(((lvar (())) . 4) ((lvar ()) . (lvar (())))))
     q))
  '((((lvar ((()))) . 5) ((lvar (())) . 4) ((lvar ()) lvar (())))))

(test "evalo-let/==/empty-state-1"
  (run* (q)
    (evalo
     `(let ((empty-s '()))
        (let ((c0 '()))
          (let ((empty-state (cons empty-s c0)))
            ((== 'cat 'cat) empty-state))))
     q))
  '(((() . ()))))

(test "evalo-==/empty-state-1"
  (run* (q)
    (evalo
     `((== 'cat 'cat) empty-state)
     q))
  '(((() . ()))))

(test "evalo-==/call-goal-1"
  (run* (q)
    (evalo
     `(call/goal (== 'cat 'cat))
     q))
  '(((() . ()))))

(test "evalo-==/lvar/call/goal-1"
  (run* (q)
    (evalo
     `(call/goal (== '(lvar . ()) 'cat))
     q))
  '((((((lvar . ()) . cat)) . ()))))

(test "evalo-==/var/call/goal-1"
  (run* (q)
    (evalo
     `(call/goal (== (var '()) 'cat))
     q))
  '((((((lvar . ()) . cat)) . ()))))

(test "evalo-==/var/call/goal-2"
  (run* (q)
    (evalo
     `(call/goal (== (var ',(peano 0)) 'cat))
     q))
  '((((((lvar . ()) . cat)) . ()))))

(test "evalo-call/fresh/lambda/call/goal-1"
  (run* (q)
    (evalo
     `(call/goal
       (call/fresh
        (lambda (my-var)
          (== my-var 'cat))))
     q))
  '((((((lvar . ()) . cat)) . (())))))

(test "evalo-call/fresh/lambda/disj/==-1"
  (time
   (run* (q)
     (evalo
      `(call/goal
        (call/fresh
         (lambda (my-var)
           (disj
            (== my-var 'cat)
            (== my-var 'dog)))))
      q)))
  '((((((lvar . ()) . cat)) . (())) ((((lvar . ()) . dog)) . (())))))

(test "evalo-call/fresh/lambda/conj/==-1"
  (time
   (run* (q)
     (evalo
      `(call/goal
        (call/fresh
         (lambda (x1)
           (call/fresh
            (lambda (x2)
              (conj
               (== x1 'cat)
               (== x2 'dog)))))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (())))))

(test "evalo-take-all/fresh/==-1"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x)
           (== x 'cat))))
      q)))
  '((((((lvar . ()) . cat)) ()))))

(test "evalo-take-all/fresh/==-2"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x y)
           (== x 'cat)
           (== 'dog y))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (())))))

(test "evalo-take-all/fresh/conde/==-1"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'cat)
              (== 'dog y))
             ((== 'rat y)
              (== x 'bat))))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (()))
     ((((lvar . ()) . bat) ((lvar . (())) . rat)) (())))))

(test "evalo-take-all/fresh/conde/==-2"
  (time
   (run* (q)
     (evalo
      `(take-all
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '((((((lvar . (())) . 1) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 1) ((lvar . ()) . b)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . b)) (())))))




(test "evalo-take/fresh/==-1a"
  (time
   (run* (q)
     (evalo
      `(take '(())
        (call/goal
         (fresh (x)
           (== x 'cat))))
      q)))
  '((((((lvar . ()) . cat)) ()))))

(test "evalo-take/fresh/==-1b"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 1)
        (call/goal
         (fresh (x)
           (== x 'cat))))
      q)))
  '((((((lvar . ()) . cat)) ()))))

(test "evalo-take/fresh/==-2"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 1)
        (call/goal
         (fresh (x y)
           (== x 'cat)
           (== 'dog y))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (())))))

(test "evalo-take/fresh/conde/==-1"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 2)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'cat)
              (== 'dog y))
             ((== 'rat y)
              (== x 'bat))))))
      q)))
  '((((((lvar . (())) . dog) ((lvar . ()) . cat)) (()))
     ((((lvar . ()) . bat) ((lvar . (())) . rat)) (())))))

(test "evalo-take/fresh/conde/==-2"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 4)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '((((((lvar . (())) . 1) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 1) ((lvar . ()) . b)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . b)) (())))))

(test "evalo-take/fresh/conde/==-2b"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 3)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '((((((lvar . (())) . 1) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 2) ((lvar . ()) . a)) (()))
     ((((lvar . (())) . 1) ((lvar . ()) . b)) (())))))

(test "evalo-take/fresh/conde/==-2c"
  (time
   (run* (q)
     (evalo
      `(take ',(peano 0)
        (call/goal
         (fresh (x y)
           (conde
             ((== x 'a))
             ((== x 'b)))
           (conde
             ((== y '1))
             ((== y '2))))))
      q)))
  '(()))







(test "evalo-reify-name-1"
  (time
   (run* (q)
     (evalo
      `(reify-name ',(peano 2))
      q)))
  '((__ ((())))))

(test "evalo-reify-s-1"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '5
        '())
      q)))
  '(()))

(test "evalo-reify-s-2"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '(lvar . ())
        '())
      q)))
  '((((lvar . ()) . (__ ())))))

(test "evalo-reify-s-3"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '(lvar . ((((())))))
        '())
      q)))
  '((((lvar . ((((()))))) . (__ ())))))

(test "evalo-reify-s-4"
  (time
   (run* (q)
     (evalo
      `(reify-s
        '((lvar . (())) 5 (lvar . ()) cat (lvar . (())))
        '())
      q)))
  '((((lvar . ()) . (__ (()))) ((lvar . (())) . (__ ())))))

(test "evalo-reify-1st-1"
  (time
   (run* (q)
     (evalo
      `(map
        reify-1st
        (take-all
         (call/goal
          (fresh (x)
            (== x 'cat)))))
      q)))
  '((cat)))

(test "evalo-reify-1st-2"
  (time
   (run* (q)
     (evalo
      `(map
        reify-1st
        (take-all
         (call/goal
          (fresh (x)
            (conde
              ((== x 'cat))
              ((== x 'dog)))))))
      q)))
  '((cat dog)))

(test "evalo-run*-1"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (== q 'cat))
      q)))
  '((cat)))

(test "evalo-run*-2"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (conde
           ((== q 'cat))
           ((== q 'dog))))
      q)))
  '((cat dog)))

(test "evalo-run*-3"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (== 'cat 'cat))
      q)))
  '(((__ ()))))

(test "evalo-run*-4"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse) (dog mouse))))

(test "evalo-run-1a"
  (time
   (run* (q)
     (evalo
      `(run ',(peano 1) (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse))))

(test "evalo-run-1b"
  (time
   (run* (q)
     (evalo
      `(run ',(peano 2) (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse) (dog mouse))))

(test "evalo-run-1c"
  (time
   (run* (q)
     (evalo
      `(run ',(peano 3) (q)
         (fresh (w v)
           (== (list w v) q)
           (== 'mouse v)
           (conde
             ((== 'cat w))
             ((== 'dog w)))))
      q)))
  '(((cat mouse) (dog mouse))))

(test "evalo-run*-let-1"
  (time
   (run* (q)
     (evalo
      `(run* (q)
         (let ((g (== 'cat q)))
           g))
      q)))
  '((cat)))

(test "evalo-run*-let-2"
  (time
   (run* (q)
     (evalo
      `(let ((f (lambda (x) (== x 'cat))))
         (run* (y)
           (let ((g (f y)))
             g)))
      q)))
  '((cat)))

(test "evalo-run*-let-3"
  (time
   (run* (q)
     (evalo
      `(let ((f (lambda (x) (cons x x))))
         (run* (y)
           (== (f 'cat) y)))
      q)))
  '(((cat . cat))))