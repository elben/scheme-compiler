;; Incrementing Scheme compiler.

(define wordsize 4) ; bytes

;;;;;;;;;;;;;;;;;;;;;;;
;; Representations
;;;;;;;;;;;;;;;;;;;;;;;

;; Scheme fixnums.
;;
;; Fixnums always have #b00 in the two least-significant bits. The
;; most-significant 30-bits represent the actual value of the fixnum. This means
;; our fixnums have a range of −229 ≤ n ≤ 229 − 1 or −536870912 ≤ n ≤ 536870911.
;; The mask, tag and shift are used together to check if a word is a fixnum, and
;; to shift appropriately to get the true value.
(define fx-shift 2)
(define fx-mask  #x03)
(define fx-tag   #x00)

(define null   #x3F)    ;; 0b0...00111111

(define bool-mask #x3F) ;; 0b0...00111111
(define bool-f #x2F)    ;; 0b0...00101111
(define bool-t #x6F)    ;; 0b0...01101111
(define bool-tag bool-f)
(define bool-bit 6)     ;;        x <-- where bit differs between true/false.
(define bool-xor #x40)  ;; 0b0...01000000

;; Chars uses two bytes. Lower byte is 0b00001111. Upper byte is the ASCII
;; value. For example, #\a is ASCII value 97. So the value is:
;; 0110 0001 0000 1111
(define char-shift 8)
(define char-mask  #xFF) ;; 0b11111111
(define char-tag   #x0F) ;; 0b00001111

;; Num bits for fixnum
(define fixnum-bits (- (* wordsize 8) fx-shift))

;; Lower/upper bounds for fixnums
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;

(define (if? expr)
  (and (= 4 (length expr)) (eq? (car expr) 'if)))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

;; Returns true if x is an immediate constant. Fixnums, booleans, null (empty
;; list), and chars are immediate constants.
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

;; Convert a value (e.g. fixnum 3) to our representation (e.g. 3 with tags).
;; ash - shift left
(define (immediate-rep x)
  (cond
    ;; Left-shift the fixnum to add our tag (0b00)
    [(fixnum? x) (ash x fx-shift)]

    ;; Just return the value
    [(boolean? x) (if x bool-t bool-f)]

    ;; Just return the value
    [(null? x) null]

    ;; Left-shift ASCII value to proper position, then add char tag via bit-wise
    ;; OR (fxlogior).
    [(char? x) (fxlogior (ash (char->integer x) char-shift) char-tag)]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'foo "foo")))

;; Is primitive function call?
(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

;; Emits immediate constants into return register %eax.
(define (emit-immediate expr)
  (emit "    movl $~s, %eax" (immediate-rep expr)))

;; TODO
(define (check-primcall-args primitive-fn args)
  #t)

;; Emits primitive method call. e.g. (boolean? x).
(define (emit-primcall expr)
  (let ([primitive-fn (car expr)]
        [args (cdr expr)])
    ;; TODO implement check-primcall-args
    (check-primcall-args primitive-fn args)
    (apply (primitive-emitter primitive-fn) args)))

;; Takes any expression and emits the bytecode.
(define (emit-expr expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(if? expr)        (emit-if expr)]
    [(primcall? expr)  (emit-primcall expr)]
    [else (error 'expr "invalid expression; don't know what to do")]))

(define (emit-function-header fn-header)
  (emit "    .text")
  (emit "    .globl ~a" fn-header)
  (emit "    .type ~a, @function" fn-header)
  (emit "~a:" fn-header))

;; Compares two values and emit boolean to eax.
;;
;; Sets eax true if given fixnum is equal to the value in the argument.
;; Otherwise, sets to false.
;;
;; If two arguments are given, the first one is placed in the %eax register to
;; compare.
;;
;; If one argument is given, function assumes the value to check against is
;; alredy in the %eax register.
(define emit-compare-bool
  (case-lambda
    [(arg val)
     (begin
       ;; Put value in %eax return register.
       (emit-expr arg)
       (emit-compare-bool val))]

     [(val)
      (begin
        ;; Compare result (in %al) with fx-tag.
        (emit "    cmp $~s, %al" val)
        (emit-boolean-flag-check))]))

;; Emit boolean value into %eax. Emits bool-t if the compare flag is equal (zero).
;; Otherwise, emits bool-f
(define (emit-boolean-flag-check)
  ;; If the compared values above were equal, sete will set the argument
  ;; register (%al) 1. Otherwise, set to 0.
  ;;
  ;; A small glitch here is that the sete instruction only sets a 16-bit register.
  ;; To work around this problem, we use the movzbl instruction that sign-extends
  ;; the lower half of the register to the upper half. Since both 0 and 1 have 0 as
  ;; their sign bit, the result of the extension is that the upper bits will be all
  ;; zeros.
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")

  ;; Shift left by bool-bit. bool-bit is where the bit differs between bool-f
  ;; and bool-t.
  ;;
  ;; Then OR with bool-f so that %al turns into a boolean value,
  ;; either bool-t or bool-f
  ;;
  ;; false: 0b00101111
  ;; true:  0b01101111
  ;;
  (emit "    sal $~s, %al" bool-bit)
  (emit "    or $~s, %al" bool-f))

;; Checks if a value is tagged with tag, given a mask. Emits boolean value into
;; %eax.
;;
;; If three arity, given value val is placed in %eax first.
;; If two arity, assumes value is in %eax.
;;
(define emit-check-tag
  (case-lambda
    [(mask tag val)
     (begin
       ;; Put value in %eax return register.
       (emit-expr val)
       (emit-check-tag mask tag))]

    [(mask tag)
     (begin
       ;; Mask %eax to get tag Put result in %eax.
       (emit "    and $~s, %eax" mask)
       ;; Returns boolean (on %eax register). True if %al register equals to tag.
       ;; False otherwise.
       (emit-compare-bool tag))]))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if expr)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr (if-test expr))
    (emit "    cmp $~s, %al" bool-f)

    ;; Jump to the alternative (else) branch if test expression was false.
    (emit "    je ~a" alt-label)

    ;; This is the consequence (then) branch.
    (emit-expr (if-conseq expr))
    (emit "    jmp ~a" end-label)

    (emit "~a:" alt-label)
    (emit-expr (if-altern expr))

    (emit "~a:" end-label)))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))

;; Macro? that allows us to set properties in a symbol's property list.
;;
;; That is, the properties *is-prim*, *arg-count* and *emitter* are stored in the symbol
;; themselves.
;;
;; > (putprop 'foo '*is-prim* #t)
;; > (getprop 'foo '*is-prim*)
;; #t
;;
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
         (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
         (lambda (arg* ...) b b* ...)))]))

(define (compile-program expr)
  ; (unless (immediate? expr) (error 'expr "expr has invalid type"))
  (emit-function-header "scheme_entry")
  (emit-expr expr)
  (emit "    ret"))

;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;

(define-primitive (fxadd1 arg)
  ;; Put arg on eax register
  (emit-expr arg)
  ;; Add 1 to fixnum. Actually adds 4, since fixnums tag is 0b11.
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 arg)
  ;; Put arg on eax register
  (emit-expr arg)
  ;; Subtract 1 to fixnum. Actually subtracts 4, since fixnums tag is 0b11.
  (emit "    addl $~s, %eax" (immediate-rep -1)))

(define-primitive (fixnum->char arg)
  ;; Put arg on eax register
  (emit-expr arg)
  ;; Left-shift fixnum to be in correct position for char, since lower bytes of
  ;; char is tagged as 0b00001111.
  (emit "    shll $~s, %eax" (- char-shift fx-shift))
  ;; Tag lower bytes with char's tag.
  (emit "    orl $~s, %eax" char-tag))

(define-primitive (char->fixnum arg)
  ;; Put arg on %eax register
  (emit-expr arg)
  ;; Shift right to move char into fixnum position. This will leave the last
  ;; two bits as 00b, which is the fixnum tag.
  (emit "    shrl $~s, %eax" (- char-shift fx-shift)))

(define-primitive (fxzero? arg)
  (emit-compare-bool arg 0))

(define-primitive (null? arg)
  (emit-compare-bool arg null))

(define-primitive (fixnum? arg)
  (emit-check-tag fx-mask fx-tag arg))

(define-primitive (boolean? arg)
  (emit-check-tag bool-mask bool-f arg))

(define-primitive (char? arg)
  (emit-check-tag char-mask char-tag arg))

;; Returns opposite boolean if arg is a boolean. Returns #f is arg is not a
;; boolean.
(define-primitive (not arg)
  (emit-expr arg)
  ;; `not` returns #t only when arg is #f, so just check for equality with
  ;; bool-f.
  (emit-compare-bool arg bool-f))

;; Fixnum logical not. Argument is treated as two's complement.
(define-primitive (fxlognot arg)
  (emit-expr arg)
  (emit "    shrl $~s, %eax" fx-shift)  ;; left shift
  (emit "    not %eax")                 ;; inverse all bits
  (emit "    shll $~s, %eax" fx-shift)) ;; right shift

