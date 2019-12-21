#lang racket

;;; Microcode construction functions

;; Store micro-word addresses
(define control-pla (make-vector #o1777 null))
;; Stores micro-word names, so that it's easier to visualize
(define control-pla-alias (make-vector #o1777 null))

;; Determines the next free micro-address
(define (get-next-address)
  (define (get-addr s)
    (if (null? (vector-ref control-pla s))
        s
        (get-addr (+ s 1))))
  (get-addr 1))

(define (deftype micro-address micro-word)
  (let ((t (internal-deftype micro-address micro-word)))
    (printf "Defining user-facing type ~o as ~a.\n" micro-address (vector-ref control-pla-alias micro-address))))

;; Puts the micro-word into the pla-vector at a given address
(define (internal-deftype micro-address micro-word)
  (vector-set! control-pla micro-address micro-word)
  micro-address)

;; We will define the micro-words for these user-facing types later
(define-syntax-rule (defchip micro-alias micro-address)
  (begin (vector-set! control-pla-alias micro-address (quote micro-alias))
         (define micro-alias (internal-deftype micro-address #o000))))

;; These all do the same thing, though they are defined slightly
;; differently in the original spec.  All put a micro-word into the
;; pla-vector at an address determined with get-next-address
(define-syntax-rule (defntype micro-alias micro-word)
  (begin (vector-set! control-pla-alias (get-next-address) (quote micro-alias))
         (define micro-alias (internal-deftype (get-next-address) micro-word))))

(define-syntax-rule (defreturn micro-alias micro-word)
  (begin (vector-set! control-pla-alias (get-next-address) (quote micro-alias))
         (define micro-alias (internal-deftype (get-next-address) micro-word))))

(define-syntax-rule (defpc micro-alias micro-word)
  (begin (vector-set! control-pla-alias (get-next-address) (quote micro-alias))
         (define micro-alias (internal-deftype (get-next-address) micro-word))))

;;; Bit manipulation methods.  None of these actually set!, just
;;; produce new numbers--don't be confused by the set and unset.
;;; [31 use |30 type 24|23 datum 0]
;;; Racket doesn't have logical shifts, of course, so these are all arithmetic

(define (use? x) (= (bitwise-and x #o20000000000) #o20000000000))
(define (use-set x) (bitwise-ior x #o20000000000))
(define (use-unset x) (bitwise-and x #o17777777777))

(define (trace? x) (= (bitwise-and x #o20000000000) #o20000000000))
(define (trace-set x) (bitwise-ior x #o20000000000))
(define (trace-unset x) (bitwise-and x #o17777777777))

(define (type-set x type) (bitwise-ior (type-shift type) (bitwise-and x #o20077777777)))
(define (type-shift x) (arithmetic-shift x 24))
(define (type-unshift x) (arithmetic-shift (use-unset x) -24))
(define (type x) (bitwise-and x #o17700000000))
(define (p-type? x) (= (bitwise-and x #o10000000000) #o10000000000))
(define (type-equal? x t) (= (type x) t))

(define (type+use x) (bitwise-and x #o37700000000))

(define (datum x) (bitwise-and x #o00077777777))
(define (datum-set x y) (bitwise-ior (type+use x) (datum y)))

;; For use in constructing environment frames with the exp register.
(define (displacement x) (arithmetic-shift (datum x) 11))
(define (set-displacement x d)
  (bitwise-ior
   (type+use x)
   (arithmetic-shift d 11)
   (frame x)))
(define (frame x) (bitwise-and x #o3777))
(define (set-frame x f)
  (bitwise-ior
   (type+use x)
   (arithmetic-shift (displacement x) 11)
   f))

;;; Chip feature definitions.

;; stores state of FSM--currently executing type
(define state #o0)

;; Pads
(define address/data #o0)
(define gc-needed #o0)
(define interrupt-request #o0)

;;; Memory

;; Make our memory.  Our memory is controlled off chip, of course, but
;; it makes sense for the emulator for our physical address space to
;; just be the maximum virtual address space of the chip--2^24 cells.
(define mem (make-vector #o100000000 (cons #o0 #o0)))

;; In essence, our memory controller.  This gets kind of mixed into
;; our hardware definitions--in reality, this would be more
;; complicated, with interrupts and the memory controller having to
;; read from the virtual address register.  This is a pain, however,
;; and pointless for emulation.
(define (mem-set! addr acar acdr) (vector-set! mem addr (cons acar acdr)))
(define (mem-car addr) (car (vector-ref mem addr)))
(define (mem-cdr addr) (cdr (vector-ref mem addr)))
(define (mem-set-car! addr acar)
  (vector-set! mem addr (cons acar (cdr (vector-ref mem addr)))))
(define (mem-set-cdr! addr acdr)
  (vector-set! mem addr (cons (car (vector-ref mem addr)) acdr)))

;;; Microcode helper functions, that would be implemented in hardware
;;; differently then normal hardware operations on the chip

(define-syntax-rule (assign reg source) (set! reg source))
(define (fetch reg) reg)

(define (go-to s) (set! state s))

(define (micro-call maddr1 maddr2)
  ;; This is storing the micro-return address maddr1 in type field of *retpc-count-mark*
  (&set-type *retpc-count-mark* maddr2)
  (go-to maddr1))

(define (micro-return)
  ;; This returns to the micro-return address maddr1 in type field of *retpc-count-mark*
  (go-to (type-unshift (fetch *retpc-count-mark*))))

(define (save quantity)
  (assign *stack* (&cons quantity (fetch *stack*))))

(define-syntax-rule (restore register)
  (begin (assign register (&car (fetch *stack*)))
         (assign *stack* (&cdr (fetch *stack*)))))

(define (dispatch reg)
  (go-to (type-unshift (fetch reg))))

(define (dispatch-on-exp-allowing-interrupts)
  (dispatch *exp*))

(define (dispatch-on-stack)
  (dispatch *stack*))

(define (eval-exp-popj-to type)
  (&set-type *stack* type)
  (save *stack*)
  (dispatch-on-exp-allowing-interrupts))

;;; Hardware operations

;; Pure register operations
(define (&=type? reg type) (type-equal? reg (type-shift type)))
(define-syntax-rule (&set-type reg type)
  (set! reg (type-set reg type)))

(define (&pointer? addr1) (p-type? addr1))
(define (&get-interrupt-routine-pointer)
  (when (= interrupt-request #o1) (datum address/data)))

(define (&eq-val reg) (= *val* reg))
(define (&val=0?) (= (datum *val*) 0))

;; Memory operations
(define (&cons reg1 reg2)
  (cond ((= *newcell* *memtop*)
         (display "Sorry, garbage collection isn't implemented yet.\n")
         (go-to done))
        (true
         (&rplaca *newcell* reg1)
         (&rplacd *newcell* reg2)
         (set! *newcell* (add1 *newcell*))
         (type-set (sub1 *newcell*) (type-unshift (type *stack*))))))

(define (&car reg) (fetch (mem-car (datum reg))))
(define (&cdr reg) (fetch (mem-cdr (datum reg))))
(define &global-value &car)

(define (&rplaca reg1 reg2) (mem-set-car! (datum reg1) reg2))
(define (&rplacd reg1 reg2) (mem-set-cdr! (datum reg1) reg2))
(define &set-global-value &rplaca)

;; Used with frames and basic arithmetic
(define (&frame=0?) (= (frame *exp*) #o0))
(define (&decrement-frame) (set! *exp* (- *exp* #o1)))

(define (&displacement=0?) (= (displacement *exp*) #o0))
(define (&decrement-displacement)
  (set! *exp* (set-displacement *exp* (sub1 (displacement *exp*)))))

(define (&decrement-scan-down-to-val) (set! *val* (sub1 *scan-down*)))
(define (&increment-scan-up-to-val) (set! *val* (add1 *scan-up*)))
(define (&val-displacement-to-exp-displacement)
  (set! *exp* (set-displacement *exp* (displacement *val*))))
(define (&val-frame-to-exp-frame)
  (set! *exp* (set-frame *exp* (frame *val*))))

;; Used by garbage collector -- these are untested, as I haven't
;; implemented garbage collector yet.  Regardless, they need some
;; work, we shouldn't be using any bit ops directly in our hardware
;; operations.

(define (&rplaca-and-mark! addr1 addr2) (&rplaca addr1 addr2) (&mark-in-use! addr1))

(define (&in-use? reg) (use? reg))
(define (&mark-in-use! addr1) (vector-set! mem addr1 (cons (use-set (mem-car addr1)) (mem-cdr addr1))))
(define (&unmark! addr1) (vector-set! mem addr1 (cons (use-unset (mem-car addr1)) (mem-cdr addr1))))

(define (&car-being-traced? addr1) (trace? (mem-cdr addr1)))
(define (&mark-car-being-traced! addr1) (vector-set! mem addr1 (cons (mem-car addr1) (trace-set (mem-cdr addr1)))))
(define (&mark-car-trace-over! addr1) (vector-set! mem addr1 (cons (mem-car addr1) (trace-unset (mem-cdr addr1)))))

(define (&clear-gc-needed) (set! gc-needed #o0))

(define (&scan-down=0?) (= *scan-down* #o0))

(define-syntax-rule (&increment-scan-up) (set! *scan-up* (add1 *scan-up*)))
(define-syntax-rule (&decrement-scan-down) (set! *scan-down* (sub1 *scan-down*)))
(define (&scan-up=scan-down?) (= *scan-up* *scan-down*))

;;; Microcode definition!  This is for the most part identical to the
;;; paper's, with the exception of the macros used to define things,
;;; and a couple tiny changes.

;; User-facing types.
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

;; Micro-word simulation definitions

;; Boot load sequence
(deftype boot-load
  (lambda () 
    (assign *scan-up* (fetch *nil*))
    (&increment-scan-up) ;; also makes *newcell* 1
    (assign *memtop* (&car (fetch *scan-up*)))
    (assign *scan-up* (fetch *memtop*))
    (assign *stack* (&get-interrupt-routine-pointer))
    (&set-type *stack* boot-load-return)
    (go-to boot-load-return))) ;; should be mark

(defreturn boot-load-return
  (lambda ()
    (assign *exp* (&car (fetch *stack*)))
    (assign *stack* (fetch *nil*))
    (&set-type *stack* done)
    (dispatch-on-exp-allowing-interrupts)))

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
    (print-reg '*stack-top* *stack-top*)
    (cond ((&=type? (fetch *stack-top*) gc-special-type)
           (go-to sweep))
          (true (assign *leader* (fetch *stack-top*))
                (cond ((&car-being-traced? (fetch *leader*))
                       (&mark-car-being-traced! (fetch *leader*))
                       (assign *stack-top* (&car (fetch *leader*)))
                       (&rplaca-and-mark! (fetch *leader*)
                                          (fetch *node-pointer*))
                       (assign *node-pointer* (fetch *leader*))
                       (go-to trace-cdr))
                      (true (assign *stack-top* (&cdr (fetch *leader*)))
                            (&rplacd (fetch *leader*) (fetch *node-pointer*))
                            (assign *node-pointer* (fetch *leader*))
                            (go-to up-trace)))))))

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
    (cond ((&scan-up=scan-down?) (go-to relocate-pointers))
          ((&in-use? (fetch *scan-down*)) (go-to scan-up-for-hole))
          (true (go-to scan-down-for-thing)))))

(defpc scan-up-for-hole
  (lambda () 
    (cond ((&in-use? (fetch *scan-up*))
           (&increment-scan-up)
           (cond ((&scan-up=scan-down?) (go-to relocate-pointers))
                 (true (go-to scan-up-for-hole))))
          (true (go-to swap-thing-and-hole)))))

(defpc swap-thing-and-hole
  (lambda ()
    (print-reg '*scan-down* *scan-down*) 
    (&rplaca-and-mark! (fetch *scan-up*) (&car (fetch *scan-down*)))
    (&rplacd (fetch *scan-up*) (&cdr (fetch *scan-down*)))
    (&rplaca (fetch *scan-down*) (fetch *scan-up*))
    (go-to scan-down-for-thing)))

(defpc relocate-pointers
  (lambda ()
    (assign *rel-tem-1* (&car (fetch *scan-down*)))
    (cond ((&pointer? (fetch *rel-tem-1*))
           (assign *rel-tem-2* (&car (fetch *rel-tem-1*)))
           (cond ((&=type? (fetch *rel-tem-2*) gc-special-type)
                  (&set-type *rel-tem-2* (fetch *rel-tem-1*))))))
    (assign *rel-tem-1* (&cdr (fetch *scan-down*)))
    (cond ((&pointer? (fetch *rel-tem-1*))
           (assign *rel-tem-2* (&car (fetch *rel-tem-1*)))
           (cond ((&=type? (fetch *rel-tem-2*) gc-special-type)
                  (&set-type *rel-tem-2* (fetch *rel-tem-1*))
                  (&rplacd (fetch *scan-down*) (fetch *rel-tem-2*))))))
    (&unmark! (fetch *scan-down*))
    (cond ((&scan-down=0?)
           (&set-type *scan-up* self-evaluating-pointer)
           (assign *stack* (&car (fetch *nil*)))
           (&rplaca (fetch *nil*) (fetch *nil*))
           (assign *val* (fetch *nil*))
           (dispatch-on-stack))
          (true (&decrement-scan-down)
                (go-to relocate-pointers)))))

;; Variable management
(deftype local
  (lambda ()
    (micro-call lookup-exp local-return)))

(defpc local-return
  (lambda ()
    (assign *val* (&car (fetch *display*)))
    (dispatch-on-stack)))

(deftype tail-local
  (lambda ()
    (micro-call lookup-exp tail-local-return)))

(defpc tail-local-return
  (lambda ()
    (assign *val* (fetch *display*))
    (dispatch-on-stack)))

(deftype global
  (lambda ()
    (assign *val*
            (&global-value (fetch *exp*)))
    (dispatch-on-stack)))

(deftype set-local
  (lambda ()
    (micro-call lookup-exp set-local-return)))

(defpc set-local-return
  (lambda ()
    (&rplaca (fetch *display*) (fetch *val*))
    (dispatch-on-stack)))

(defntype set-tail-local
  (lambda ()
    (micro-call lookup-exp set-tail-local-return)))

(defpc (set-tail-local-return)
  (lambda ()
    (&rplacd (fetch *display*) (fetch *val*))
    (dispatch-on-stack)))

(deftype set-only-tail-local
  (lambda ()
    (if (&frame=0?)
        (begin (&rplaca (fetch *display*) (fetch *val*))
               (dispatch-on-stack))
        (begin (assign *display* (&cdr (fetch *display*)))
               (&decrement-frame)
               (go-to set-only-tail-local)))))

(deftype set-global
  (lambda ()
    (&set-global-value (fetch *exp*) (fetch *val*))
    (dispatch-on-stack)))

(defpc lookup-exp
  (lambda ()
    (if (&frame=0?)
        (begin (assign *display* (&car (fetch *display*)))
               (go-to count-displacement))
        (begin (&decrement-frame)
               (assign *display* (&cdr (fetch *display*)))
               (go-to lookup-exp)))))

(defpc count-displacement
  (lambda ()
    (if (&displacement=0?)
        (micro-return)
        (begin (&decrement-displacement)
               (assign *display* (&cdr (fetch *display*)))
               (go-to count-displacement)))))

;; The user can choose for these to be different things, like ints,
;; floats, etc.
(deftype self-evaluating-immediate
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-immediate-1
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-immediate-2
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-immediate-3
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-immediate-4
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype symbol
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(defntype self-evaluating-pointer
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-pointer-1
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-pointer-2
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-pointer-3
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype self-evaluating-pointer-4
  (lambda ()
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

;; Basic procedure definition
(deftype procedure
  (lambda ()
    (assign *val* (&cons (fetch *exp*) (fetch *display*)))
    (&set-type *val* closure)
    (dispatch-on-stack)))

;; Conditional, handels non substition evaluation (special forms)
(deftype conditional
  (lambda ()
    (if (&eq-val (fetch *nil*))
        (assign *exp* (&cdr (fetch *exp*)))
        (assign *exp* (&car (fetch *exp*))))
    (dispatch-on-exp-allowing-interrupts)))

;; Micro return address stored in *retpc-count-mark*, which will be
;; stuck on the stack. Then we evaluate the car.  This is not
;; microcode, its a helper function.  This gets used extensively by
;; eval and apply
(define (save-cdr-and-eval-car return-tag)
  (begin (&set-type *retpc-count-mark* return-tag)
         (go-to standard-eval)))

;; Function evaluation

(defpc standard-eval
  (lambda ()
    (save (fetch *display*))
    (&set-type *stack* (type-unshift (type (fetch *retpc-count-mark*)))) ;; we need this different cause of way we did types
    (save (&cdr (fetch *exp*)))
    (&set-type *stack* standard-return)
    (assign *exp* (&car (fetch *exp*)))
    (dispatch-on-exp-allowing-interrupts)))

(defreturn standard-return
  (lambda ()
    (restore *exp*)
    (assign *retpc-count-mark* (fetch *stack*))
    (restore *display*)
    (dispatch (fetch *retpc-count-mark*))))

;; Function with multiple arguments packages them in a sequence
(deftype sequence
  (lambda ()
    (assign *val* (fetch *nil*))
    (save-cdr-and-eval-car sequence-return)))

(defreturn sequence-return
  (lambda ()
    (dispatch-on-exp-allowing-interrupts)))

(deftype get-control-point
  (lambda ()
    (assign *val* (&cons (fetch *stack*) (fetch *nil*)))
    (&set-type *val* control-point)
    (save-cdr-and-eval-car sequence-return)))

;; Eval arg1...argn-1. argn-1 is the body of the procedure being executed
(deftype first-argument
  (lambda ()
    (save-cdr-and-eval-car first-argument-return)))

(defreturn first-argument-return
  (lambda ()
    ;; val has been set by standard-eval earlier.  We start making a list of arguments here,
    ;; *args* -> (*val* . *nil*)
    (assign *args* (&cons (fetch *val*) (fetch *nil*)))
    (save (fetch *args*))
    (dispatch-on-exp-allowing-interrupts)))

(deftype next-argument
  (lambda ()
    (save (fetch *args*))
    (save-cdr-and-eval-car next-argument-return)))

(defreturn next-argument-return
  (lambda ()
    (restore *args*)
    ;; *args* -> (val . (val . nil)), or more
    (&rplacd (fetch *args*) (&cons (fetch *val*) (fetch *nil*)))
    (assign *args* (&cdr (fetch *args*)))
    (dispatch-on-exp-allowing-interrupts)))

(deftype last-argument
  (lambda ()
    (save (fetch *args*))
    (save-cdr-and-eval-car last-argument-return)))

(defreturn last-argument-return
  (lambda ()
    (restore *args*)
    (&rplacd (fetch *args*)
             (&cons (fetch *val*) (fetch *nil*)))
    ;; popj is not self documenting in 2019...
    ;; its the return from subroutine instruction on a PDP10
    (eval-exp-popj-to internal-apply)))

(deftype apply-1-arg
  (lambda ()
    (save-cdr-and-eval-car apply-1-arg-return)))

(defreturn apply-1-arg-return
  (lambda ()
    (assign *args* (&cons (fetch *val*) (fetch *nil*)))
    (save (fetch *args*))
    (eval-exp-popj-to internal-apply)))

(deftype apply-no-args
  (lambda ()
    (assign *exp* (&car (fetch *exp*)))
    (save (fetch *nil*))
    (eval-exp-popj-to internal-apply)))

;; This is normal apply--for any number of arguments.
(deftype spread-argument
  (lambda ()
    (save-cdr-and-eval-car spread-argument-return)))

(defreturn spread-argument-return
  (lambda ()
    (save (fetch *val*))
    (eval-exp-popj-to internal-apply)))

(defreturn internal-apply
  (lambda ()
    (restore *args*)
    (assign *exp* (fetch *val*))
    (dispatch-on-exp-allowing-interrupts)))

;; Procedures get turned into closures in their environment.
(deftype closure
  (lambda ()
    (assign *display*
            (&cons (fetch *args*) (&cdr (fetch *exp*))))
    (assign *exp* (&car (&car (fetch *exp*)))) ;; 2 cars because documentation is in first
    (dispatch-on-exp-allowing-interrupts)))

;; Used for special forms

(deftype control-point
  (lambda ()
    (assign *val* (&car (fetch *args*))) ;; This arg will be used by constructor of control point
    (assign *stack* (&car (fetch *exp*)))
    (dispatch-on-stack)))

(deftype interrupt-point
  (lambda ()
    (assign *stack* (fetch *exp*))
    (restore *val*)
    (restore *display*)
    (go-to restore-exp-args-dispatch)))

;; Primitive functions

(deftype primitive-apply-1
  (lambda ()
    (save (&cdr (fetch *exp*)))
    (assign *exp* (&car (fetch *exp*)))
    (eval-exp-popj-to primitive-apply-1-return)))

(defreturn primitive-apply-1-return
  (lambda ()
    (restore *exp*)
    (dispatch-on-exp-allowing-interrupts)))

(deftype primitive-car
  (lambda ()
    (assign *val* (&car (fetch *val*)))
    (dispatch-on-stack)))

(deftype primitive-cdr
  (lambda ()
    (assign *val* (&cdr (fetch *val*)))
    (dispatch-on-stack)))

(deftype primitive-type?
  (lambda ()
    (assign *exp* (fetch *val*))
    (assign *val* (fetch *nil*))
    (&set-type *val* (fetch *exp*))
    (dispatch-on-stack)))

(deftype primitive-not-atom
  (lambda ()
    (if (&pointer? (fetch *val*))
        (begin (assign *val* (fetch *nil*))
               (&set-type *val* self-evaluating-immediate))
        (assign *val* (fetch *nil*)))
    (dispatch-on-stack)))

;; Functions used with peano arithmetic, ideally the user would have a
;; seperate coprocessor to do math (think Intel 8087), and interface
;; with it using interrupts.

(deftype primitive-zerop
  (lambda ()
    (if (&val=0?)
        (begin (assign *val* (fetch *nil*))
               (&set-type *val* self-evaluating-immediate))
        (assign *val* (fetch *nil*)))
    (dispatch-on-stack)))

(deftype primitive-sub1
  (lambda ()
    (assign *scan-down* (fetch *val*))
    (&decrement-scan-down-to-val)
    (dispatch-on-stack)))

(deftype primitive-add1
  (lambda ()
    (assign *exp* (fetch *scan-up*))
    (assign *scan-up* (fetch *val*))
    (&increment-scan-up-to-val)
    (assign *scan-up* (fetch *exp*))
    (dispatch-on-stack)))

(deftype primitive-displacement-add1
  (lambda ()
    (assign *exp* (fetch *nil*))
    (&decrement-frame)
    (&val-displacement-to-exp-displacement)
    (assign *args* (fetch *scan-up*))
    (assign *scan-up* (fetch *exp*))
    (&increment-scan-up)
    (assign *exp* (fetch *scan-up*))
    (&val-frame-to-exp-frame)
    (assign *val* (fetch *exp*))
    (dispatch-on-stack)))

;; Back to normal primitve functions.

(deftype primitive-apply-2
  (lambda ()
    (save (fetch *args*))
    (save (&cdr (fetch *exp*)))
    (assign *exp* (&car (fetch *exp*)))
    (eval-exp-popj-to restore-exp-args-dispatch)))

(defreturn restore-exp-args-dispatch
  (lambda ()
    (restore *exp*)
    (restore *args*)
    (dispatch-on-exp-allowing-interrupts)))

(deftype primitive-cons
  (lambda ()
    (&rplacd (fetch *args*) (fetch *val*))
    (restore *val*)
    (dispatch-on-stack)))

(deftype primitive-eq
  (lambda ()
    (restore *args*)
    (assign *args* (&car (fetch *args*)))
    (if (&eq-val (fetch *args*))
        (begin (assign *val* (fetch *nil*))
               (&set-type *val* self-evaluating-immediate))
        (assign *val* (fetch *nil*)))
    (dispatch-on-stack)))

(deftype primitive-rplaca
  (lambda ()
    (restore *args*)
    (assign *val* (&rplaca (&car (fetch *args*)) (fetch *val*)))
    (dispatch-on-stack)))

(deftype primitive-rplacd
  (lambda ()
    (restore *args*)
    (assign *val* (&rplacd (&car (fetch *args*)) (fetch *val*)))
    (dispatch-on-stack)))

(deftype primitive-type!
  (lambda ()
    (restore *args*)
    (assign *exp* *val*)
    (assign *val* (&car (fetch *args*)))
    (&set-type *val* (fetch *exp*))
    (dispatch-on-stack)))

;; Finally, our interrupt handler!

(deftype process-interrupt
  (lambda ()
    (save (fetch *args*))
    (save (fetch *exp*))
    (save (fetch *display*))
    (save (fetch *val*))
    (&set-type *stack* interrupt-point)
    (assign *args* (fetch *stack*))
    (assign *exp* (&car (&get-interrupt-routine-pointer)))
    (dispatch-on-exp-allowing-interrupts)))

(define (make-car t d)
  (bitwise-ior (type-shift t) d))

(define make-cdr make-car)

(define (cycle state)
  ((vector-ref control-pla state)))

;;; Functions used with CLI

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

(define (get-control-alias type)
  (vector-ref control-pla-alias type))

(define (print-reg alias v)
  (printf "~a: ~o | in-use: ~o | type: ~o ~a | datum: ~o\n"
          alias
          v
          (if (use? v) 1 0)
          (type-unshift (type v))
          (get-control-alias (type-unshift (type v)))
          (datum v)))

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

(define (print-state)
  (printf "State: ~o ~a\n\n" state (get-control-alias state)))

(define (run)
  (unless (= state done)
    (cycle state)
    (run)))

(define (step)
  (cond ((= state done)
         (print-state)
         (print-registers)
         (print-mem (+ *newcell* #o2)))
        (true
         (print-state)
         (print-registers)
         (print-mem (+ *newcell* #o2))
         (cycle state)
         (display "Type s to step: ")
         (read)
         (step))))

(define (initialize-scheme-79)
  (display "\nInitializing scheme-79 with values:\n")
  (printf "Memtop: ~o\n" (mem-car #o1))
  (printf "Initial procedure is at memory location: ~o\n" address/data)
  (go-to boot-load)
  (printf "\nStarting boot load sequence...\n\n"))

;;; Memory initialization.  This is all standard.
(mem-set! #o0 #o0 #o0)       ;; NIL
(mem-set! #o1 #o1000 #o0)    ;; initial *MEMTOP*
(set! address/data #o1001)   ;; interrrupt vector, has location of proc to exec
(set! interrupt-request #o1) ;; we will read an interrupt

;;; Test programs.  TODO: interface to read and write programs more easily.

;; (mem-set! #o1001 (make-car self-evaluating-immediate-1 #o11) #o0)

;; Take car of #o2000
;; (mem-set! #o1001 (make-car primitive-apply-1 #o1002) #o0)
;; (mem-set! #o1002 (make-car self-evaluating-pointer-1 #o2000) (make-cdr primitive-car #o0))
;; (mem-set! #o2000 (make-car self-evaluating-immediate-1 #o11) (make-cdr self-evaluating-immediate-1 #o12))

;; Take cdr of #o2000
;; (mem-set! #o1001 (make-car primitive-apply-1 #o1002) #o0)
;; (mem-set! #o1002 (make-car self-evaluating-pointer-1 #o2000) (make-cdr primitive-cdr #o0))
;; (mem-set! #o2000 (make-car self-evaluating-immediate-1 #o11) (make-cdr self-evaluating-immediate-1 #o12))

;;((lambda () 3))
(mem-set! #o1001 (make-car apply-no-args #o1002) #o0)
(mem-set! #o1002 (make-car procedure #o2000) #o0)
(mem-set! #o2000 (make-car self-evaluating-immediate #o3) #o0)

;; Start running our program!
(initialize-scheme-79)
(step)
