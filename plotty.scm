#!/usr/bin/ol -r

(import 
   (owl args)
   (owl date))

(define (at x y)
   (cond
      ((< x 0)
         (error "x is negative at " x))
      ((< y 0)
         (error "y is negative at " y))
      ((> x #b111111111111)
         (error "x overfow at " x))
      ((> y #b111111111111)
         (error "y overfow at " y))
      (else
         (bor (<< x 12) y))))

(define (all-keys evs)
   (keys
      (fold
         (λ (seen evt)
            (ff-fold
               (λ (seen key _)
                  (if (get seen key #f)
                     seen
                     (put seen key #t)))
               seen evt))
         #empty evs)))

(define (flatten obj tl)
   (cond
      ((pair? obj)
         (flatten (car obj)
            (flatten (cdr obj) tl)))
      ((string? obj)
         (render obj tl))
      ((null? obj)
         tl)
      (else
         (error "cannot render " obj))))

(define-syntax timed 
   (syntax-rules ()
      ((timed msg exp)
         (lets ((start (time-ms))
                (res exp)
                (elapsed (- (time-ms) start)))
               ;(print-to stderr (str msg " took " elapsed "ms"))
               res))))

(define (printer . objs)
   (write-bytes stdout
      (flatten objs null)))

(define (char->digit c)
   (let ((d (- c #\0)))
      (cond
         ((< d 0) #false)
         ((> d 9) #false)
         (else d))))

(define (data-string->number s)
   (let ((x (string->number s)))
      (if x 
         (round (* 100 (string->number s)))
         x)))

(define (data->number lst)
   (let loop ((lst lst) (n 0))
      (if (null? lst)
         (* n 100)
         (lets ((hd lst lst)
                (d (- hd #\0)))
            (cond
               ((eq? hd #\.)
                  (cond
                     ((null? lst)
                        (* n 100))
                     ((null? (cdr lst))
                        (+ (* n 100) (* (- (car lst) #\0) 10)))
                     (else
                        (+ (* n 100)
                           (+ (* (- (car lst) #\0) 10) (- (cadr lst) #\0))))))
               (else
                  (loop  lst (+ (* n 10) d))))))))

(define (data-list->number lst)
   (cond
      ((null? lst) 0)
      ((eq? (car lst) #\-)
         (- 0 (data->number (cdr lst))))
      ((eq? (car lst) #\+)
         (data->number (cdr lst)))
      (else (data->number lst))))

(define (parse-json-event s)
   (lets 
      ((s (s/[{"}]//g s)) ;; remove quotes and leading and trailing braces
       (pairs (c/, / s)) ;; split around , 
       (key-vals 
          (map 
             (λ (x) 
                (let ((key-val (c/:/ x)))
                   (cons (string->symbol (car key-val))
                         (data-string->number (cadr key-val)))))
              pairs)))
      (list->ff key-vals)))

 (define (parse-csv-event s)
   (lets 
      ((pairs (c/[; ]/ s)) ;; split around spaces
       (key-vals 
          (map 
             (λ (x) 
                (let ((key-val (c/=/ x)))
                   (cons (string->symbol (car key-val))
                         ;(data-string->number (cadr key-val))
                         42
                         )))
              pairs)))
      (list->ff key-vals)))

(define parse-event 
   parse-csv-event)

(define (parse-events old ls)
   (let ((res (lfold
      (λ (out event-line)
         (cons (parse-event event-line) out))
      old ls)))
      res))

(define (read-events file)
   (display-to stderr (str "parsing " file "... "))
   (let ((port (open-input-file file)))
      (if port
         (let ((data (parse-events null (lines port))))
            (if data
               (begin
                  (print-to stderr (str " read " (length data) " events"))
                  data)
               null))
         null)))

(define usage "usage: plotty [args] [file] ...")

;; cutter : st char lst  cook → st' ((cook seq) ...) 
(define (cutter st char lst cook)
   (let loop ((st st) (lst lst) (rout null) (done null))
      (cond
         ((null? lst)
            (if (null? rout)
               (values st done)
               (lets ((st val (cook st rout)))
                  (values st (cons val done)))))
         ((eq? (car lst) char)
            (lets ((st val (cook st rout)))
               (loop st (cdr lst) null (cons val done))))
         (else
            (loop st (cdr lst) (cons (car lst) rout) done)))))

(define (cut-at-= st lst cook)
   (let loop ((st st) (lst lst) (rout null))
      (cond
         ((null? lst)
            (error "cut-at: no =" lst))
         ((eq? (car lst) #\=)
            (cook st rout (cdr lst)))
         (else
            (loop st (cdr lst) (cons (car lst) rout))))))

(define (lookup st key)
   (cond
      ((null? st) #false)
      ((equal? (car (car st)) key)
         (cdr (car st)))
      (else (lookup (cdr st) key))))

(define (intern st key)
   (let ((res (lookup st key)))
      (if res
         (values st res)
         (let ((sym (string->symbol (list->string (reverse key)))))
            (values (append st (list (cons key sym))) sym)))))
 
(define (parse-port evs port)
   (lets ((st res
      (cutter null #\newline
         (timed "input streaming" (force-ll (port->byte-stream port)))
         (λ (st rline)
            (cutter st #\space
               rline
               (λ (st field)
                  (cut-at-= st field
                     (λ (st rhead tl)
                        (lets ((st key (intern st rhead))
                               (val (data-list->number tl))
                               (node (cons key val)))
                           (values st node))))))))))
      (map list->ff res)))
   
(define (parse-port-old evs port)
   (parse-events evs (lines port)))

(define (maybe-open-input-file path)
   (cond
      ((equal? path "-")
         stdin)
      (else
         (open-input-file path))))

(define (parse-inputs args)
   (fold
      (λ (evs path)
         (cond
            ((not evs) evs)
            ((maybe-open-input-file path) =>
               (λ (port)
                  (timed "parsing port" (parse-port evs port))))
            (else
               (print-to stderr "Failed to open " path)
               #false)))
      null args))

(define (render-output args events x-key y-key)
   (lets
      ((data 
          (fold
             (λ (out node)
                (let ((x (getf node x-key))
                      (y (getf node y-key)))
                   (if (and x y)
                      (cons (cons x y) out)
                      out)))
             null events))
       (xs (map car data))
       (ys (map cdr data))
       (width (get args 'width 10))
       (height (get args 'height 10)))
      (if (null? data)
         'empty
         (lets 
           ((xmin (fold min (car xs) (cdr xs)))
            (xmax (fold max (car xs) (cdr xs)))
            (xstep (/ (- xmax xmin) width))
            (ymin (fold min (car ys) (cdr ys)))
            (ymax (fold max (car ys) (cdr ys)))
            (yrange (- ymax ymin))
            (ystep (/ (- ymax ymin) height)))
           (print (date-str (time)) ": " x-key " x " y-key " " (round (/ ymin 100)) " - " (round (/ ymax 100)))
           (fold
              (λ (pixels node)
                 (lets ((x (round (/ (- (car node) xmin) xstep)))
                        (y (round (/ (- (cdr node) ymin) ystep)))
                        (y (- height y)))
                    (put pixels (at x y) #true)))
              (-> empty
                 (put 'ymax ymax)
                 (put 'ymin ymin)
                 (put 'xmax xmax)
                 (put 'xmin xmin)
                 (put 'width (get args 'width 10))
                 (put 'height (get args 'height 10)))
              (zip cons xs ys))))))
        
(define command-line-rules
   (cl-rules
      `((help "-h" "--help"  
            comment "show this thing")
        (keys "-k" "--keys" cook ,(λ (s) (map string->symbol (c/, */ s)))
            comment "comma-separated list of keys to pick (default all)")
        (output "-o" "--output" has-arg
            comment "output csv file")
        (x "-x" "--x-axis" has-arg
            comment "key or column of x axis in data ")
        (y "-y" "--y-axis" has-arg
           comment "key or column of y axis in data")
        (width "-W" "--width"
           has-arg cook ,string->integer 
           default "160")
        (height "-H" "--height"
           has-arg cook ,string->integer
           default "40")
        (ids "-i"  "--ids"   cook ,(λ (s) (map data-string->number (c/, */ s))) 
            comment "comma-separated list of ids to keep (default all)"))))

;; ... a b ...
;, ... c d ...

(define full #\█)

(define (pixels->char a b c d)
   (if a
      (if b
         (if c
            (if d #\█ #\▛)
            (if d #\▜ #\▀))
         (if c
            (if d #\▙ #\▌)
            (if d #\▚ #\▘)))
      (if b
         (if c
            (if d #\▟ #\▞)
            (if d #\▐ #\▝))
         (if c
            (if d #\▄ #\▖)
            (if d #\▗ #\space)))))

(define (render-row data x y width)
   (if (< x width)
      (lets
         ((a (getf data (at x y)))
          (b (getf data (at (+ x 1) y)))
          (c (getf data (at x (+ y 1))))
          (d (getf data (at (+ x 1) (+ y 1)))))
         (cons (pixels->char a b c d) 
            (render-row data (+ x 2) y width)))
      null))


(define top-left-corner #\╭)     ;; or ┌ ╭
(define top-right-corner #\╮)    ;; or ┐
(define bottom-left-corner #\╰)  ;; or └
(define bottom-right-corner #\╯) ;; or ┘

(define y-border-tick #\┤)
(define y-border #\│)
   
(define (top-line width)
   (list->string
      (cons top-left-corner
         (append
            (map (λ (x) #\─) (iota 0 1 (round (/ width 2))))
            (list top-right-corner)))))

(define (bottom-line width)
   (list->string
      (cons bottom-left-corner
         (append
            (map (λ (x) #\─) (iota 0 1 (round (/ width 2))))
            (list bottom-right-corner)))))


(define (align-right val w)
   (cond
      ((number? val)
         (align-right (render val null) w))
      ((string? val)
         (align-right (string->list val) w))
      ((< (length val) w)
         (align-right (cons #\space val) w))
      (else 
         (list->string val))))

(define (unfloat int)
   (lets ((q r (quotrem int 100)))
      (if (= r 0)
         (str q)
         (str q "." r))))

(define (print-picture data)
   (if (eq? data 'empty)
      (print-to stderr "(no data)")
      (lets ((w (getf data 'width))
             (h (getf data 'height))
             (ymax (getf data 'ymax))
             (ymin (getf data 'ymin))
             (y-axis-scale-width 
                (fold max 0 
                   (map (o string-length unfloat) 
                      (list ymax ymin))))
             (left-pad (list->string (map (λ (x) #\space) (iota 0 1 y-axis-scale-width)))))
         (print (align-right null y-axis-scale-width) (top-line w))
         (let loop ((y 0))
            (cond
               ((= y 0)
                  (display (align-right (unfloat ymax) y-axis-scale-width))
                  (print (list->string (cons y-border-tick (append (render-row data 0 y w) (list y-border)))))
                  (loop (+ y 2)))
               ((>= (+ y 1) h)
                  (display (align-right (unfloat ymin) y-axis-scale-width))
                  (print (list->string (cons y-border-tick (append (render-row data 0 y w) (list y-border)))))
                  (print left-pad (bottom-line w)))
               (else
                  (display (align-right null y-axis-scale-width))
                  (print (list->string (cons y-border (append (render-row data 0 y w) (list y-border)))))
                  (loop (+ y 2))))))))

(define (dot pixels x y)
   (put pixels (+ (<< x 12) y #true)))

(define (distance a b)
   (abs (- a b)))

(define (nearest evt keys val)
   (let loop ((evt evt) (keys (cdr keys)) (lead (cons (car keys) (distance (getf evt (car keys)) val))))
      (if (null? keys)
         (car lead)
         (let ((this (distance (getf evt (car keys)) val)))
            (loop evt (cdr keys)
               (if (< this (cdr lead))
                  (cons (car keys) this)
                  lead))))))

(define (guess-x evs)
   (let ((evs (take evs 30)))
      (let ((opts (fold intersect (keys (car evs)) (map keys (cdr evs)))))
         (cond
            ((null? opts)
               (error "cannot guess x" "please specify one with -x"))
            ((= (length opts) 1)
               (car opts))
            (else
               (nearest (car evs) opts (* (time) 100)))))))
           
(define (maybe op arg)
   (if arg (op arg) arg))

(define (render-outputs args events)
   (if (pair? events)
      (let ((x-key (or (maybe string->symbol (getf args 'x)) (guess-x events))))
         (for-each
            (λ (y-key)
               ;(print-to stderr (str "rendering graph of " x-key " x " y-key))
               (print-picture
                  (render-output args events x-key y-key)))
            (lets ((y-key (getf args 'y)))
               (if y-key
                  (map string->symbol 
                     (c/,/ y-key))
                  (diff (all-keys events) (list x-key))))))))

(λ (args)
   (process-arguments (cdr args) command-line-rules "command line fail"
      (λ (opts args)
         (cond
            ((getf opts 'help)
               (print usage)
               (print (format-rules command-line-rules))
               0)
            ((null? args)
               (print "I need some data")
               1)
            ((timed "input parsing" (parse-inputs args)) =>
               (λ (events)
                  (render-outputs opts events)))
            (else
               (print-to stderr "Failed ot load event data")
               1)))))


