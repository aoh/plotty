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
               (print-to stderr (str msg " took " elapsed "ms"))
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
   (round (* 100 (string->number s))))

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
                         (data-string->number (cadr key-val)))))
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

(define (parse-port evs port)
   (parse-events evs (lines port)))
   
(define (parse-inputs args)
   (fold
      (λ (evs path)
         (cond
            ((not evs) evs)
            ((open-input-file path) =>
               (λ (port)
                  (parse-port evs port)))
            (else
               (print-to stderr "Failed to open " path)
               #false)))
      null args))
         
(define (render-output args events)
   (lets
      ((x-key (getf args 'x))
       (y-key (getf args 'y))
       (data 
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
       (width (getf args 'width))
       (height (getf args 'height))
       (xmin (fold min (car xs) (cdr xs)))
       (xmax (fold max (car xs) (cdr xs)))
       (xstep (/ (- xmax xmin) width))
       (ymin (fold min (car ys) (cdr ys)))
       (ymax (fold max (car ys) (cdr ys)))
       (yrange (- ymax ymin))
       (ystep (/ (- ymax ymin) height)))
      (fold
         (λ (pixels node)
            (let ((x (round (/ (- (car node) xmin) xstep)))
                  (y (round (/ (- (cdr node) ymin) ystep))))
               (put pixels (at x y) #true)))
         (-> empty
            (put 'width (get args 'width 10))
            (put 'height (get args 'height 10)))
         (zip cons xs ys))))

(define command-line-rules
   (cl-rules
      `((help "-h" "--help"  
            comment "show this thing")
        (keys "-k" "--keys" cook ,(λ (s) (map string->symbol (c/, */ s)))
            comment "comma-separated list of keys to pick (default all)")
        (output "-o" "--output" has-arg
            comment "output csv file")
        (x "-x" "--x-axis" cook ,string->symbol default "0"
            comment "key or column of x axis in data ")
        (y "-y" "--y-axis" cook ,string->symbol default "1"
           comment "key or column of y axis in data")
        (width "-w" "--width"
           has-arg cook ,string->integer 
           default "160")
        (height "-h" "--height"
           has-arg cook ,string->integer
           default "60")
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

(define (top-line width)
   (list->string
      (cons #\┌
         (append
            (map (λ (x) #\─) (iota 0 1 (round (/ width 2))))
            (list #\┐)))))

(define (bottom-line width)
   (list->string
      (cons #\└
         (append
            (map (λ (x) #\─) (iota 0 1 (round (/ width 2))))
            (list #\┘)))))

(define border #\│)

(define (print-picture data)
   (let ((w (getf data 'width))
         (h (getf data 'height)))
      (print (top-line w))
      (let loop-row ((y 0))
         (cond
            ((< y h)
               (print (list->string (cons border (append (render-row data 0 y w) (list border)))))
               (loop-row (+ y 2)))
            (else
               (print (bottom-line w)))))))

(define (dot pixels x y)
   (put pixels (+ (<< x 12) y) #true))

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
                  (print (date-str (time)))
                  (print-picture
                     (render-output opts events))))
            (else
               (print-to stderr "Failed ot load event data")
               1)))))


