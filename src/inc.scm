(load "compiler.scm")
(load "tests-driver.scm")

(define args (command-line))
(define program-file (cadr args))
(define binary-file
  (if (> (length args) 2)
    ;; User-given binary file
    (caddr args)

    ;; By default, if the file is "myprog.scm", output binary as "myprog.scm.out"
    (string-append program-file ".out")))

(let ([port (open-input-file program-file)])
  (let ([program (read port)])
    (build-program program (string-append program-file ".s") binary-file)
    (close-input-port port)))
