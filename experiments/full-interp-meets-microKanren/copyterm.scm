(load "load.scm")

(define var=? var-eq?)

(define (var-member? x l)
  (and (pair? l)
       (or (var=? x (car l))
           (var-member? x (cdr l)))))
           
(define (union l1 l2)
  (if (null? l1)
      l2
      (let ((rec (union (cdr l1) l2)))
        (if (var-member? (car l1) rec)
            rec
            (cons (car l1) rec)))))

(define (term-variables t)
  (if (pair? t)
      (union
       (term-variables (car t))
       (term-variables (cdr t)))
      (if (var? t)
          (list t)
          '())))
    
(define (term-variableso t v)
  (project (t)
    (== (term-variables t) v)))

;; An important one-to-one relation
(define (zipo l1 l2 zip)
  (conde
    ((== '() l1) (== '() l2) (== '() zip))
    ((fresh (a1 a2 d1 d2 rec)
       (== (cons a1 d1) l1)
       (== (cons a2 d2) l2)
       (== (cons (cons a1 a2) rec) zip)
       (zipo d1 d2 rec)))))

;; Create a new term with the same structure, but
;; replace each variable with its associated variable
;; defined in alist `a`.
(define (copy-term t a)
  (if (pair? t)
      (cons
        (copy-term (car t) a)
        (copy-term (cdr t) a))
      (if (var? t)
          (cdr (assp (lambda (t^) (var=? t t^)) a))
          t)))

;; WEB `term-variableso` projects `t1`, so there are two calls to
;; project on `t1`, although both are local in scope.
(define (copy-termo t1 t2)
  (fresh (v1 v2 a)
    (term-variableso t1 v1)
    (zipo v1 v2 a)
    (project (t1 a)
      (== (copy-term t1 a) t2))))

(test "copy-termo/v1"
  (run* (a b)
    (fresh (x y z)
      (== `(8 ,y (,x) 9) z)
      (== `(5 ,x (,x 6 ,y) 7 ,z ,y ,x) a)
      (copy-termo a b)))
  '(((5 _.0 (_.0 6 _.1) 7 (8 _.1 (_.0) 9) _.1 _.0)
     (5 _.2 (_.2 6 _.3) 7 (8 _.3 (_.2) 9) _.3 _.2))))

(test "copy-termo/v2"
  (letrec ((var-member? (lambda (x l)
                          (and (pair? l)
                               (or (var=? x (car l))
                                   (var-member? x (cdr l)))))))
        
    (letrec ((union (lambda (l1 l2)
                      (if (null? l1)
                          l2
                          (let ((rec (union (cdr l1) l2)))
                            (if (var-member? (car l1) rec)
                                rec
                                (cons (car l1) rec)))))))
          
      (letrec ((term-variables (lambda (t)
                                 (if (pair? t)
                                     (union
                                      (term-variables (car t))
                                      (term-variables (cdr t)))
                                     (if (var? t)
                                         (list t)
                                         '())))))
            
        (letrec ((copy-term (lambda (t a)
                              (if (pair? t)
                                  (cons
                                    (copy-term (car t) a)
                                    (copy-term (cdr t) a))
                                  (if (var? t)
                                      (cdr (assp (lambda (t^) (var=? t t^)) a))
                                      t)))))
              
          (letrec ((term-variableso (lambda (t v)
                                      (project (t)
                                        (== (term-variables t) v)))))
                
            (letrec ((zipo (lambda (l1 l2 zip)
                             (conde
                               ((== '() l1) (== '() l2) (== '() zip))
                               ((fresh (a1 a2 d1 d2 rec)
                                  (== (cons a1 d1) l1)
                                  (== (cons a2 d2) l2)
                                  (== (cons (cons a1 a2) rec) zip)
                                  (zipo d1 d2 rec)))))))
                  
              (letrec ((copy-termo (lambda (t1 t2)
                                     (fresh (v1 v2 a)
                                       (term-variableso t1 v1)
                                       (zipo v1 v2 a)
                                       (project (t1)
                                         (project (a)
                                           (== (copy-term t1 a) t2)))))))
                  
                (run* (a b)
                  (fresh (x y z)
                    (== `(8 ,y (,x) 9) z)
                    (== `(5 ,x (,x 6 ,y) 7 ,z ,y ,x) a)
                    (copy-termo a b))))))))))
  '(((5 _.0 (_.0 6 _.1) 7 (8 _.1 (_.0) 9) _.1 _.0)
     (5 _.2 (_.2 6 _.3) 7 (8 _.3 (_.2) 9) _.3 _.2))))


(test "copy-termo/midori"
  (run 1 (q)
    (evalo
     `(letrec ((var-member? (lambda (x l)
                              (and (pair? l)
                                   (or (var=? x (car l))
                                       (var-member? x (cdr l)))))))
        
        (letrec ((union (lambda (l1 l2)
                          (if (null? l1)
                              l2
                              (let ((rec (union (cdr l1) l2)))
                                (if (var-member? (car l1) rec)
                                    rec
                                    (cons (car l1) rec)))))))
          
          (letrec ((term-variables (lambda (t)
                                     (if (pair? t)
                                         (union
                                          (term-variables (car t))
                                          (term-variables (cdr t)))
                                         (if (var? t)
                                             (list t)
                                             '())))))
            
            (letrec ((copy-term (lambda (t a)
                                  (if (pair? t)
                                      (cons
                                        (copy-term (car t) a)
                                        (copy-term (cdr t) a))
                                      (if (var? t)
                                          (cdr (assp (lambda (t^) (var=? t t^)) a))
                                          t)))))
              
              (letrec ((term-variableso (lambda (t v)
                                          (project (t)
                                            (== (term-variables t) v)))))
                
                (letrec ((zipo (lambda (l1 l2 zip)
                                 (conde
                                   ((== '() l1) (== '() l2) (== '() zip))
                                   ((fresh (a1 a2 d1 d2 rec)
                                      (== (cons a1 d1) l1)
                                      (== (cons a2 d2) l2)
                                      (== (cons (cons a1 a2) rec) zip)
                                      (zipo d1 d2 rec)))))))
                  
                  (letrec ((copy-termo (lambda (t1 t2)
                                         (fresh (v1 v2 a)
                                           (term-variableso t1 v1)
                                           (zipo v1 v2 a)
                                           (project (t1)
                                             (project (a)
                                               (== (copy-term t1 a) t2)))))))
                  
                    #;(run* (a b)
                      (fresh (x y z)
                        (== `(8 ,y (,x) 9) z)
                        (== `(5 ,x (,x 6 ,y) 7 ,z ,y ,x) a)
                        (copy-termo a b)))

                    (run* (c)
                      (fresh (a b)
                        (== (list a b) c)
                        (fresh (x y z)
                          (== (list 8 y (list x) 9) z)
                          (== (list 5 x (list x 6 y) 7 z y x) a)
                          (copy-termo a b))))

                    )))))))
     q))
  '((((5 (__ ()) ((__ ()) 6 (__ (()))) 7 (8 (__ (())) ((__ ())) 9) (__ (())) (__ ()))
      (5 (__ ((()))) ((__ ((()))) 6 (__ (((()))))) 7 (8 (__ (((())))) ((__ ((())))) 9) (__ (((())))) (__ ((())))))))
  #;'(((5 _.0 (_.0 6 _.1) 7 (8 _.1 (_.0) 9) _.1 _.0)
       (5 _.2 (_.2 6 _.3) 7 (8 _.3 (_.2) 9) _.3 _.2)))
  )

(test "copy-termo/midori/debug"
  (time
   (run 1 (q)
     (evalo
        `(letrec ((var-member? (lambda (x l)
                                 (and (pair? l)
                                      (or (var=? x (car l))
                                          (var-member? x (cdr l)))))))
        
           (letrec ((union (lambda (l1 l2)
                             (if (null? l1)
                                 l2
                                 (let ((rec (union (cdr l1) l2)))
                                   (if (var-member? (car l1) rec)
                                       rec
                                       (cons (car l1) rec)))))))
          
             (letrec ((term-variables (lambda (t)
                                        (if (pair? t)
                                            (union
                                             (term-variables (car t))
                                             (term-variables (cdr t)))
                                            (if (var? t)
                                                (list t)
                                                '())))))
            
               (letrec ((copy-term (lambda (t a)
                                     (if (pair? t)
                                         (cons
                                           (copy-term (car t) a)
                                           (copy-term (cdr t) a))
                                         (if (var? t)
                                             (cdr (assp (lambda (t^) (var=? t t^)) a))
                                             t)))))
              
                 (letrec ((term-variableso (lambda (t v)
                                             (project (t)
                                               (== (term-variables t) v)))))
                
                   (letrec ((zipo (lambda (l1 l2 zip)
                                    (conde
                                      ((== '() l1) (== '() l2) (== '() zip))
                                      ((fresh (a1 a2 d1 d2 rec)
                                         (== (cons a1 d1) l1)
                                         (== (cons a2 d2) l2)
                                         (== (cons (cons a1 a2) rec) zip)
                                         (zipo d1 d2 rec)))))))
                  
                     (letrec ((copy-termo (lambda (t1 t2)
                                            (fresh (v1 v2 a)
                                              (term-variableso t1 v1)
                                              (zipo v1 v2 a)
                                              (project (t1)
                                                (project (a)
                                                  (== (copy-term t1 a) t2)))))))

                       ;; returns (#t) in 0.6 seconds
                       #;(let ((v0 (var ',(peano 0))))
                       (let ((v1 (var ',(peano 1)))) ;
                       (let ((v2 (var ',(peano 2)))) ;
                       (let ((v3 (var ',(peano 3)))) ;
                       (var-member? v2 (list v0 v1 v2 v3))))))

                       ;; returns (#f) in 0.7 seconds
                       #;(let ((v0 (var ',(peano 0))))
                       (let ((v1 (var ',(peano 1)))) ;
                       (let ((v2 (var ',(peano 2)))) ;
                       (let ((v3 (var ',(peano 3)))) ;
                       (var-member? v2 (list v0 v1 v3))))))

                       ;; returns (((lvar) (lvar ()))) after 0.7 seconds
                       #;(let ((v0 (var ',(peano 0))))
                       (let ((v1 (var ',(peano 1)))) ;
                       (let ((v2 (var ',(peano 2)))) ;
                       (let ((v3 (var ',(peano 3)))) ;
                       (union (list v0) (list v1))))))

                       ;; returns (((lvar) (lvar (())) (lvar ()) (lvar ((()))))) in 1.1 seconds
                       #;(let ((v0 (var ',(peano 0))))
                       (let ((v1 (var ',(peano 1)))) ;
                       (let ((v2 (var ',(peano 2)))) ;
                       (let ((v3 (var ',(peano 3)))) ;
                       (union (list v0 v1) (list v2 v1 v3))))))
                     
                       ;; Computed: ((5 (6) 7))
                       ;; (copy-term '(5 (6) 7) '())

                       ;; ((5 (6) 7))
                       #;(let ((v0 (var ',(peano 0))))
                         (let ((v1 (var ',(peano 1))))
                           (copy-term '(5 (6) 7) (cons (cons v0 v1) '()))))
                       
                       ;;(list '5 (list '6) '7)

                       ;; ((((lvar) . ( lvar ()))))
                       #;(let ((v0 (var ',(peano 0))))
                         (let ((v1 (var ',(peano 1))))
                           (cons (cons v0 v1) '())))    

                       ;; ((5 (6) 7))
                       #;(let ((v0 (var ',(peano 0))))
                         (let ((v1 (var ',(peano 1))))
                           (copy-term (list '5 (list '6) '7) (cons (cons v0 v1) '()))))

                       ;; ((5 (lvar ()) (6) 7))
                       #;(let ((v0 (var ',(peano 0))))
                         (let ((v1 (var ',(peano 1))))
                           (copy-term (list '5 v0 (list '6) '7) (cons (cons v0 v1) '()))))

                       ;; in 2.9 seconds
                       ;; ((5 (lvar ()) (6 (lvar ((()))) 7) (lvar ()) 8))
                       #;(let ((v0 (var ',(peano 0))))
                         (let ((v1 (var ',(peano 1))))
                           (let ((v2 (var ',(peano 2))))
                             (let ((v3 (var ',(peano 3))))
                               (copy-term
                                (list '5 v0 (list '6 v2 '7) v0 '8)
                                ;; v0->v1, v2->v3
                                (cons (cons v2 v3)
                                      (cons (cons v0 v1)
                                            '())))))))

                       ;; ((((__ ()) (__ (())))))
                       ;; in 2.6 seconds
                       #;(run* (c)
                         (fresh (a b)
                           (== (list a b) c)))

                       ;; 8 seconds
                       ;; ((((5 (__ ()) (6 (__ (())) 7) (__ ()) 8) (__ ((()))))))
                       #;(run* (c)
                         (fresh (a b t1 t2)
                           (== (list '5 a (list '6 b '7) a '8) t1)
                           (== (list t1 t2) c)
                           ;; no copy-termo call
                           ))

                       ;;after 69 seconds
                       ;;((((5 (__ ()) (6 (__ (())) 7) (__ ()) 8) (5 (__ ((()))) (6 (__ (((())))) 7) (__ ((()))) 8))))
                       (run* (c)
                         (fresh (a b t1 t2)
                           (== (list '5 a (list '6 b '7) a '8) t1)
                           (== (list t1 t2) c)
                           (copy-termo t1 t2)
                           ))

                       ;; after 21 seconds
                       #;((((5 (__ ()) (6 (__ (())) 7) (__ ()) 8) ;; t1
                          (__ ((())))                           ;; t2
                          ((__ (())) (__ ()))                   ;; v1
                          (__ (((()))))                         ;; v2
                          (__ ((((())))))                       ;; a
                          )))                       
                       #;(run* (c)
                         (fresh (v w t1 t2 v1 v2 a)
                           (== (list t1 t2 v1 v2 a) c)
                           (== (list '5 v (list '6 w '7) v '8) t1)
                           (term-variableso t1 v1)
                           ;;
                           ))

                       ;; in 60 seconds
                       #;((((5 (__ ()) (6 (__ (())) 7) (__ ()) 8) ;; t1
                          (__ ((())))                           ;; t2
                          ((__ (())) (__ ()))                   ;; v1
                          ((__ (((())))) (__ ((((()))))))       ;; v2
                          (((__ (())) __ (((())))) ((__ ()) __ ((((()))))))))) ;; a                       
                       #;(run* (c)
                         (fresh (v w t1 t2 v1 v2 a)
                           (== (list t1 t2 v1 v2 a) c)
                           (== (list '5 v (list '6 w '7) v '8) t1)
                           (term-variableso t1 v1)
                           (zipo v1 v2 a)
                           ;;
                           ))                                             
                       
                       ;; returns () after 23 seconds
                       #;(run ',(peano 1) (c)
                       (fresh (a b)     ;
                       (== (list a b) c) ;
                       (copy-termo a b)))
                     
                       #;(run* (c)
                       (fresh (a b)     ;
                       (== (list a b) c) ;
                       (fresh (x y z)   ;
                           ;;(== (list 8 y (list x) 9) z) ;
                           ;;(== (list 5 x (list x 6 y) 7 z y x) a) ;
                           ;;(copy-termo a b) ;
                                        ;
                           ;; 18 seconds to run the term-variableso call ;
                           ;; (term-variableso (list 5 x (list x 6 y) 7 z y x) a) ;
                           ;; => ((__ ()) (__ (())) (__ ((())))) ;
                           ;; as expected ;
                           ;; (equivalent to (_.0 _.1 _.2)) ;
                                        ;
                           ;; 39 seconds to run the zipo call ;
                           ;;(zipo '(a b c) '(1 2 3) a) ;
                           ;; => ((a . 1) (b . 2) (c . 3)) ;
                           ;; as expected ;
                       )))
                     
                       #;(run* (a b)
                       (fresh (x y z)     ; ; ;
                       (== `(8 ,y (,x) 9) z) ; ; ;
                       (== `(5 ,x (,x 6 ,y) 7 ,z ,y ,x) a) ; ; ;
                       (copy-termo a b))  ; ; ;
                       )
                       )))))))
        q)))
  '((((5 (__ ()) (6 (__ (())) 7) (__ ()) 8)
      (5 (__ ((()))) (6 (__ (((())))) 7) (__ ((()))) 8))))
  )
