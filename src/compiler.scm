
(define wordsize 4) ; bytes

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

(define null   #x3F) ;; 0b00111111
(define bool-f #x2F) ;; 0b00101111
(define bool-t #x6F) ;; 0b01101111

;; Chars uses two bytes. Lower byte is 0b00001111.
;; Upper byte is the ASCII value.
(define char-mask  #xFF) ;; 0b11111111
(define char-tag   #x0F) ;; 0b00001111
(define char-shift 8)

;; Num bits for fixnum
(define fixnum-bits (- (* wordsize 8) fx-shift))

;; Lower/upper bounds for fixnums
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

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

(define (compile-program x)
  (unless (immediate? x) (error 'x "x has invalid type"))
  (emit "    .text")
  (emit "    .globl scheme_entry")
  (emit "    .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "    movl $~s, %eax" (immediate-rep x))
  (emit "    ret"))

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")

; (test-all)
