
;; Scheme Example - The farmer, wolf, goat, cabbage problem.

(require racket/trace) ;; load trace function

;; this function does a breadth-first search
(define fwgc
  (λ (init goal)
    (do ( (path-list (list (list init))) ) ;; initialize the vars, we only have path-list, which is the Q
      
        ((or (null? path-list) (equal? (car goal) (caaar path-list))) ;; terminal condition
         (if (null? path-list) '() (reverse (car path-list)))) ;; return either empty or reverse of 1st path
      
        (set! path-list (extend path-list))) ;; loop body
                                             ;; path-list = extend(path-list);
   )
)

(define bfs fwgc)

(define Jugs
  (lambda (A B C)
     (fwgc (append '(0 0) (list A) (list B)) (list C))))

;; check membership in a list, including sub-list of arbitary levels
;; (in 4 '(5 ((4 6)))
(define in 
  (lambda (a l)
    (cond ((null? l) #f)
          ((equal? a (car l)) #t)
          ((list? (car l)) 
           (or (in a (car l))(in a (cdr l))))
          (else (in a (cdr l))))))


(define FillA
  (lambda (L)
     (append (list (caddr L)) (cdr L))))

(define FillB
  (lambda (L)
     (append (list (car L)) (list (cadddr L)) (cddr L))))

(define EmptyA
  (lambda (L)
    (append '(0) (cdr L))))

(define EmptyB
  (lambda (L)
    (append (list (car L)) '(0) (cddr L))))

(define AtoB
  (lambda (L)
     (cond
       ((eq? (cadr L)(cadddr L)) L)
       ((eq? (car L) 0) L)
       (else (AtoB (append (list (- (car L) 1)) (list(+ (cadr L) 1)) (cddr L))))
       )))



(define BtoA
  (lambda (L)
     (cond
       ((eq? (car L)(caddr L)) L)
       ((eq? (cadr L) 0) L)
       (else (BtoA (append (list (+ (car L) 1)) (list(- (cadr L) 1)) (cddr L))))
       )))

;; make a list of OPs so we could call them in order without having to spell them out. 
;; Can we do that in C++?
(define op-list 
  '(FillA FillB EmptyA EmptyB AtoB BtoA))
;; you can change this list to other op-list to bfs of different puzzles
;; (define op-list '(up down left right)

(define extend 
   (lambda (lst) ;; lst is the local name for path-list in fwgc
     (do ((ops op-list (cdr ops)) ;; local name for the op-list, allowing us to walk through the op-list
          (cur-node (caar lst)) ;; cur-node contains the list describing the current state such as (w w w w)
          (new-node '())) ;; initialize
       
       ((null? ops) (cdr lst))         ;; terminal condition, remove cur-node when return
       
       (set! new-node ((eval (car ops)) cur-node)) ;; this calls the function on cur-node
       (if (not (in new-node lst))
           (set! lst (append lst (list (cons new-node (car lst)))))) ; adding a new path at the end of lst
     )
   )
)


(define printjugs
  (lambda (L)
     (printjugsR L 0)))


(define printjugsR
  (lambda (L a)
     (cond
       ((eq? (length L) 1) (display "Steps Taken: ") a)
       ((eq? (caar L)(caadr L)) (printBChange L (+ a 1)))
       ((eq? (cadar L)(cadadr L)) (printAChange L (+ a 1)))
       (else (ABChange L (+ a 1)))
       )))
(define printBChange
  (lambda (L a)
     (cond
       ((eq? (cadadr L) 0) (display "Empty-B\n") (printjugsR (cdr L) a))
       (else (display "Fill-B\n") (printjugsR (cdr L) a))
       )))
(define printAChange
  (lambda (L a)
     (cond
       ((eq? (caadr L) 0) (display "Empty-A\n") (printjugsR (cdr L) a))
       (else (display "Fill-A\n") (printjugsR (cdr L) a))
       )))
(define ABChange
  (lambda (L a)
     (cond
       ((eq? (+ (caar L) 1) (caadr L)) (display "B-to-A\n") (printjugsR (cdr L) a))
       (else (display "A-to-B\n") (printjugsR (cdr L) a))
       )))



