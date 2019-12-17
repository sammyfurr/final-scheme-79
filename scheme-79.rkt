#lang racket

;; Microcode construction functions

(define control-pla (make-vector #o1777 null))

(define (get-next-address)
  (define (get-addr s)
    (if (null? (vector-ref control-pla s))
        s
        (get-addr (+ s 1))))
  (get-addr 0))

(define (deftype micro-address micro-word)
  (vector-set! control-pla micro-address micro-word)
  micro-address)

(define-syntax-rule (defchip micro-alias micro-address)
  (define micro-alias (deftype micro-address #o000)))

(define-syntax-rule (defreturn micro-alias micro-word)
  (define micro-alias (deftype (get-next-address) micro-word)))

(define-syntax-rule (defpc micro-alias micro-word)
  (define micro-alias (deftype (get-next-address) micro-word)))

;; (define in-use (arithmetic-shift 1 31))

;; (define (in-use? x) (bitwise-bit-set? x 31))

(define (type-shift x) (arithmetic-shift x 24))
(define type-clear #o20077777777)
(define (type-equal? x t) (= (type x) t))

(define (datum x) (bitwise-and x #o00077777777))
(define (datum-set x y) (bitwise-ior (type+use x) (datum y))) ;; return x with y's datum
(define (type x) (bitwise-and x #o17700000000))
(define (p-type x) (if (= (bitwise-and x #o10000000000) #o10000000000) true false))
(define (type+use x) (bitwise-and x #o37700000000))

(define (use? x) (if (= (bitwise-and x #o20000000000) #o20000000000) true false))
(define (use-set x) (bitwise-and x #o20000000000))

(define (trace? x) (if (= (bitwise-and x #o20000000000) #o20000000000) true false))
(define (trace-set x) (bitwise-ior x #o20000000000))
;; Types (op-codes)

(defchip symbol #o001)
(defchip global #o002)
(defchip set-global #o003)
(defchip conditional #o004)
(defchip procedure #o005)
(defchip first-argument #o006)
(defchip next-argument #o007)
(defchip last-argument #o010)
(defchip apply-no-args #o011)
(defchip apply-1-arg #o012)
(defchip primitive-apply-1 #o013)
(defchip primitive-apply-2 #o014)
(defchip sequence #o015)
(defchip spread-argument #o016)
(defchip closure #o017)
(defchip get-control-point #o020)
(defchip control-point #o021)
(defchip interrupt-point #o022)
(defchip self-evaluating-pointer-1 #o023)
(defchip self-evaluating-pointer-2 #o024)
(defchip self-evaluating-pointer-3 #o025)
(defchip self-evaluating-pointer-4 #o026)

(defchip self-evaluating-immediate #o100)
(defchip local #o101)
(defchip tail-local #o102)
(defchip set-local #o103)
(defchip set-only-tail #o104)
(defchip set-only-tail-local #o105)
(defchip primitive-car #o106)
(defchip primitive-cdr #o107)
(defchip primitive-cons #o110)
(defchip primitive-rplaca #o111)
(defchip primitive-rplacd #o112)
(defchip primitive-eq #o113)
(defchip primitive-type? #o114)
(defchip primitive-type! #o115)
(defchip gc-special-type #o116)
(defchip self-evaluating-immediate-1 #o117)
(defchip self-evaluating-immediate-2 #o120)
(defchip self-evaluating-immediate-3 #o121)
(defchip self-evaluating-immediate-4 #o122)
(defchip mark #o123)
(defchip done #o124)
(defchip primitive-add1 #o125)
(defchip primitive-sub1 #o126)
(defchip primitive-zerop #o127)
(defchip primitive-displacement-add1 #o130)
(defchip primitive-not-atom #o131)
(defchip boot-load #o776)
(defchip process-interrupt #o777)

;; Hardware definitions

;; Registers, on chip
(define *nil* #o0)
(define *memtop* #o0)
(define *newcell* #o0)
(define *scan-up* *newcell*)
(define *exp* #o0)
(define *scan-down* *exp*)
(define *val* #o0)
(define *rel-tem-2* *val*)
(define *stack-top* *val*)
(define *args* #o0)
(define *leader* *args*)
(define *rel-tem-1* *args*)
(define *display* #o0)
(define *node-pointer* *display*)
(define *stack* #o0)
(define *retpc-count-mark* #o0)
(define *intermediate-argument* #o0)

;; stores state of FSM
(define state #o0)

(define-syntax-rule (go-to s) (set! state s))

;; Pads
(define address/data #o0)
(define gc-needed #o0)


(define-syntax-rule (assign reg source) (set! reg (datum-set reg source)))
(define (fetch reg) reg)

;;; Memory

(define mem (make-vector #o100000000 (cons #o0 #o0))) ;; 2^24
(define (mem-set! addr acar acdr) (vector-set! mem addr (cons acar acdr)))
(define (mem-car addr) (car (vector-ref mem addr)))
(define (mem-cdr addr) (cdr (vector-ref mem addr)))
(define (mem-set-car! addr acar) (vector-set! mem addr (cons acar (cdr (vector-ref mem addr)))))
(define (mem-set-cdr! addr acdr) (vector-set! mem addr (cons (car (vector-ref mem addr)) acdr)))

;; Hardware ops

(define (&=type? addr type) (type-equal? (mem-car addr) (type-shift type)))

(define (&car addr) (fetch (mem-car addr)))
(define (&cdr addr) (fetch (mem-cdr addr)))

(define (&rplaca addr1 addr2) (mem-set-car! addr1 (mem-car addr2)))
(define (&rplacd addr1 addr2) (mem-set-cdr! addr1 (mem-cdr addr2)))
(define (&rplaca-and-mark! addr1 addr2) (&rplaca addr1 addr2) (&mark-in-use! addr1))

(define (&pointer? addr1) (p-type addr1))

(define (&in-use? addr1) (use? (mem-car addr1)))
(define (&mark-in-use! addr1) (vector-set! addr1 (use-set (mem-car addr1))))

(define (&car-being-traced? addr1) (trace? (mem-cdr addr1)))
(define (&mark-car-being-traced! addr1) (vector-set! addr1 (trace-set (mem-cdr addr1))))

(define (&clear-gc-needed) (set! gc-needed #o0))

(define-syntax-rule (&increment-scan-up) (set! *scan-up* (add1 *scan-up*)))
(define-syntax-rule (&decrement-scan-down) (set! *scan-down* (sub1 *scan-down*)))
(define (&scan-up=scan-down?) (if (= *scan-up*  *scan-down*) true false))

(define (&get-interrupt-routine-pointer) (fetch address/data))
(define-syntax-rule (&set-type reg type) (set! reg (bitwise-ior (type-shift type) reg)))

;;; Micro-word simulation definitions

;; Boot load sequence

(deftype boot-load
  (lambda () 
    (assign *scan-up* (fetch *nil*))
    (&increment-scan-up)
    (assign *memtop* (&car (fetch *scan-up*)))
    (assign *scan-up* (fetch *memtop*))
    (assign *stack* (&get-interrupt-routine-pointer))
    (&set-type *stack* boot-load-return)
    (go-to done))) ;; should be mark

(defreturn boot-load-return
  (lambda ()
    (assign *exp* (&car (fetch *stack*)))
    (assign *stack* (fetch *nil*))
    (&set-type *stack* done)))

(deftype done
  (lambda ()
    (go-to done)))

;; Garbage collection words

(deftype mark
  (lambda () 
    (&rplaca (fetch *nil*) (fetch *stack*))
    (assign *node-pointer* (fetch *nil*))
    (assign *stack-top* (fetch *nil*))
    (&set-type *stack-top* gc-special-type)
    (go-to mark-node)))

(defpc mark-node
  (lambda ()
    (assign *leader* (&car (fetch *node-pointer*)))
    (cond ((and (&pointer? (fetch *leader*))
                (not (&in-use? (fetch *leader*))))
           (&mark-car-being-traced! (fetch *node-pointer*))
           (&rplaca-and-mark! (fetch *node-pointer*) (fetch *stack-top*))
           (go-to down-trace))
          (true
           (&mark-in-use! (fetch *node-pointer*))
           (go-to trace-cdr)))))

(defpc down-trace
  (lambda ()
    (assign *stack-top* (fetch *node-pointer*))
    (assign *node-pointer* (fetch *leader*))
    (go-to mark-node)))

(defpc trace-cdr
  (lambda ()
    (assign *leader* (&cdr (fetch *node-pointer*)))
    (cond ((and (&pointer? (fetch *leader*))
                (not (&in-use? (fetch *leader*))))
           (&rplacd (fetch *node-pointer*) (fetch *stack-top*))
           (go-to down-trace))
          (true
           (go-to up-trace)))))

(defpc up-trace
  (lambda ()
    (cond ((&=type? (fetch *stack-top*) gc-special-type)
           (go-to sweep))
          (true))))

(defpc sweep
  (lambda ()
    (&increment-scan-up)
    (assign *scan-down* (fetch *scan-up*))
    (assign *scan-up* (fetch *nil*))
    (&set-type *scan-up* gc-special-type)
    (&clear-gc-needed)
    (go-to scan-down-for-thing)))

(defpc scan-down-for-thing
  (lambda ()
    (&decrement-scan-down)
    (cond ((&scan-up=scan-down?) (go-to relocate-pointers)))))

(defpc relocate-pointers
  (lambda ()
    (assign *rel-tem-1* (&car (fetch *scan-down*)))
    (cond ((&pointer? (fetch *rel-tem-1*))
           (assign *rel-tem-2* (&car (fetch *rel-tem-1*)))
           (cond ((&=type? (fetch *rel-tem-2*) gc-special-type)
                  (&set-type *rel-tem-2* (fetch *rel-tem-1*))))))))

;; Mem-init
(mem-set! #o0 #o0 #o0)
(mem-set! #o1 #o1000 #o0)

(define (cycle state)
  ((vector-ref control-pla state)))


;; Debug functions
(define (print-mem-addr addr)
  (printf "~o car: ~o cdr: ~o\n" addr (mem-car addr) (mem-cdr addr)))

(define (print-mem c)
  (define (mp i)
    (unless (= i c)
      (print-mem-addr i)
      (mp (add1 i))))
  (display "Memory:\n")
  (mp 0)
  (newline))

(define (print-reg alias v)
  (printf "~a: ~o | in-use: ~o | type: ~o | datum: ~o\n" alias v (use-set v) (type v) (datum v)))

(define (print-registers)
  (display "Registers:\n")
  (print-reg '*nil *nil*)
  (print-reg '*memtop* *memtop*)
  (print-reg '*newcell* *newcell*)
  (print-reg '*scan-up* *scan-up*)
  (print-reg '*exp* *exp*)
  (print-reg '*scan-down* *scan-down*)
  (print-reg '*val* *val*)
  (print-reg '*rel-tem-2* *rel-tem-2*)
  (print-reg '*stack-top* *stack-top*)
  (print-reg '*args* *args*)
  (print-reg '*leader* *leader*)
  (print-reg '*rel-tem-1* *rel-tem-1*)
  (print-reg '*display* *display*)
  (print-reg '*node-pointer* *node-pointer*)
  (print-reg '*stack* *stack*)
  (print-reg '*retpc-count-mark* *retpc-count-mark*)
  (print-reg '*intermediate-argument* *intermediate-argument*)
  (newline))

(define (run)
  (unless (= state done)
    (cycle state)
    (run)))

(print-registers)
(print-mem 10)
(go-to boot-load)
(run)
(print-registers)
(print-mem 10)
