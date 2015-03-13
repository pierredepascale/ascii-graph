#lang racket

(define (graph-ref g x y)
  (if (< y (vector-length g))
      (let ((line (vector-ref g y)))
        (if (< x (string-length line))
            (string-ref line x)
            #\space))
      #\space))

(define (ascii-graph-lines graph) (vector-length graph))
(define (ascii-graph-columns graph) (string-length (vector-ref graph 0)))

(define (line-start? graph x y)
  (member (graph-ref graph x y) '(#\* #\+)))

(define (line-end? graph x y)
  (let ((ch (graph-ref graph x y)))
    (member ch '(#\> #\< #\^ #\v #\+))))

(define (line-part? graph x y)
  (not (char=? #\space (graph-ref graph x y))))

(define (horizontal-line-start? graph x y)
  (and (line-start? graph x y)
       (line-part? graph (+ x 1) y)))
       
(define (vertical-line-start? graph x y)
  (and (line-start? graph x y)
       (line-part? graph x (+ y 1))))

(define (vertical-line-end? graph x y)
  (and (line-end? graph x y)
       (char=? #\space (graph-ref graph x (+ y 1)))))

(define (horizontal-line-end? graph x y)
  (and (line-end? graph x y)
       (char=? #\space (graph-ref graph (+ x 1) y))))

(define (parse-ascii-graph graph)
  (let ((maxx (ascii-graph-columns graph))
        (maxy (ascii-graph-lines graph)))
    (let lp ((x 0) (y 0) (objects '()))
      (cond ((= x maxx) (lp 0 (+ y 1) objects))
            ((= y maxy) objects)
            (else (let* ((objects (if (horizontal-line-start? graph x y)
                                      (cons (parse-horizontal-line graph x y)
                                            objects)
                                      objects))
                         (objects (if (vertical-line-start? graph x y)
                                      (cons (parse-vertical-line graph x y)
                                            objects)
                                      objects)))
                    (lp (+ x 1) y objects)))))))

(define (parse-horizontal-line graph x y)
  (let lp ((ex (+ x 1)) (ey y))
    (cond ((line-end? graph ex ey) (list 'line x y ex ey))
          ((char=? #\- (graph-ref graph ex ey)) (lp (+ ex 1) ey))
          (else (error "bad line character at " ex ey)))))

(define (parse-vertical-line graph x y)
  (let lp ((ex x) (ey (+ y 1)))
    (cond ((line-end? graph ex ey) (list 'line x y ex ey))
          ((char=? #\| (graph-ref graph ex ey)) (lp ex (+ ey 1)))
          (else (error "bad line character at " ex ey)))))

(define *graph* (vector "   +----+  "
                        "   |    +->"
                        "   +----+  "))

(define (test) (parse-ascii-graph *graph*))