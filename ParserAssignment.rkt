#lang racket

;; Track line numbers and tokens for better error reporting
(define current-line (make-parameter 1))
(define token-line-map (make-hash))

;; Enhanced error reporting
(define (syntax-error msg [token #f] [line #f])
  (error (format "Syntax error~a~a: ~a"
                (if line (format " at line ~a" line) "")
                (if token (format " near '~a'" token) "")
                msg)))

;; Improved tokenizer with line tracking
(define (combine-comparison-ops tokens)
  (let loop ([tokens tokens] [result '()])
    (if (null? tokens)
        (reverse result)
        (let ([t1 (car tokens)])
          (if (and (not (null? (cdr tokens)))
                   (let ([t2 (cadr tokens)])
                     (and (equal? (car t1) "=") (equal? (car t2) "=")
                          (equal? (cadr t1) (cadr t2)))))
              (loop (cddr tokens) (cons (list "==" (cadr t1)) result))
              (loop (cdr tokens) (cons t1 result)))))))

(define (tokenize-with-lines input)
  (let loop ([chars (string->list input)]
             [tokens '()]
             [current ""]
             [line 1])
    (if (null? chars)
        (begin
          (when (not (string=? current ""))
            (hash-set! token-line-map current line))
          (reverse (if (string=? current "") tokens (cons (list current line) tokens))))
        (let ([c (car chars)] [next-c (if (null? (cdr chars)) #f (cadr chars))])
          (cond
            ;; Handle $$ as a special case first
            [(and (char=? c #\$) (char=? next-c #\$))
             (when (not (string=? current ""))
               (hash-set! token-line-map current line))
             (hash-set! token-line-map "$$" line)
             (loop (cddr chars)
                   (append (if (string=? current "")
                              '()
                              (cons (list current line) tokens))
                           (list (list "$$" line)))
                   ""
                   line)]
            [(char-whitespace? c)
             (when (not (string=? current ""))
               (hash-set! token-line-map current line))
             (loop (cdr chars)
                   (if (string=? current "")
                       tokens
                       (cons (list current line) tokens))
                   ""
                   (if (char=? c #\newline) (add1 line) line))]
            [(member c '(#\( #\) #\; #\+ #\- #\= #\< #\> #\!))
             (when (not (string=? current ""))
               (hash-set! token-line-map current line))
             (hash-set! token-line-map (string c) line)
             (loop (cdr chars)
                   (append (if (string=? current "")
                              '()
                              (cons (list current line) tokens))
                           (list (list (string c) line)))
                   ""
                   line)]
            [else
             (loop (cdr chars)
                   tokens
                   (string-append current (string c))
                   line)])))))

(define (tokenize input)
  (let* ([raw-tokens (tokenize-with-lines input)]
         [combined (combine-comparison-ops raw-tokens)])
    (map car combined)))

;;Parsing Functions
(define (parse-id tokens)
  (if (null? tokens)
      (syntax-error "Unexpected end of input" #f (current-line))
      (let ([token (car tokens)])
        (if (regexp-match? #px"^[a-zA-Z][a-zA-Z0-9]*$" token)
            (begin
              (current-line (hash-ref token-line-map token (current-line)))
              token)
            (syntax-error "Expected identifier" token (hash-ref token-line-map token (current-line)))))))

;; Enhanced number parsing
(define (parse-num tokens)
  (if (null? tokens)
      (syntax-error "Unexpected end of input" #f (current-line))
      (let ([token (car tokens)])
        (if (regexp-match? #px"^[+-]?\\d+$" token)
            (begin
              (current-line (hash-ref token-line-map token (current-line)))
              token)
            (syntax-error "Expected number" token (hash-ref token-line-map token (current-line)))))))

(define (parse-if-stmt tokens)
  (if (and (equal? (car tokens) "if")
           (equal? (cadr tokens) "("))
      (let ([expr (parse-expr (cddr tokens))])
        (if (equal? (car expr) "expr")
            (let ([stmt-list (parse-stmt-list (cdr expr))])
              (if (equal? (car stmt-list) "endif;")
                  (list 'if-stmt expr stmt-list)
                  (error "Syntax error: Missing 'endif;'")))
            (error "Syntax error: Invalid if condition")))
      (error "Syntax error: Malformed if statement")))

(define (parse-stmt tokens)
  (cond
    [(equal? (car tokens) "if") (parse-if-stmt tokens)]
    [(equal? (car tokens) "read") (parse-read-stmt tokens)]
    [(equal? (car tokens) "write") (parse-write-stmt tokens)]
    [else (parse-assign-stmt tokens)]))

;;Supporting Parsing Functions
(define (parse-expr tokens)
  (cond
    [(parse-id tokens) => (λ (id) (list 'expr id (parse-etail (cdr tokens))))]
    [(parse-num tokens) => (λ (num) (list 'expr num (parse-etail (cdr tokens))))]
    [else (syntax-error "Expected expression" (if (null? tokens) #f (car tokens)) (current-line))]))

(define (parse-etail tokens)
  (if (and (not (null? tokens))
           (or (equal? (car tokens) "+")
               (equal? (car tokens) "-")
               (equal? (car tokens) "<")
               (equal? (car tokens) "<=")
               (equal? (car tokens) ">")
               (equal? (car tokens) ">=")
               (equal? (car tokens) "==")
               (equal? (car tokens) "!=")))
      (list 'etail (car tokens) (parse-expr (cdr tokens)))
      '()))
(define (parse-program tokens)
  (let ([stmt-list (parse-stmt-list tokens)])
    (cond
      [(null? tokens) (error "Syntax error: Unexpected end of input - missing '$$'")]
      [(equal? (car tokens) "$$") (list 'program stmt-list)]
      [else (error (format "Syntax error: Expected '$$' but found '~a'" (car tokens)))])))

(define (parse-stmt-list tokens)
  (cond
    [(or (null? tokens) (equal? (car tokens) "$$")) '()]
    [else
     (let ([stmt (parse-stmt tokens)])
       (cons stmt (parse-stmt-list (cdr tokens))))]))

;; Read statement parser
(define (parse-read-stmt tokens)
  (if (equal? (car tokens) "read")
      (let ([id (parse-id (cdr tokens))])
        (if (equal? (cadr id) ";")
            (list 'read-stmt (car id))
            (error "Syntax error: Missing semicolon after read statement")))
      (error "Syntax error: Expected 'read'")))

;; Write statement parser
(define (parse-write-stmt tokens)
  (if (equal? (car tokens) "write")
      (let ([expr (parse-expr (cdr tokens))])
        (if (equal? (cadr expr) ";")
            (list 'write-stmt (car expr))
            (error "Syntax error: Missing semicolon after write statement")))
      (error "Syntax error: Expected 'write'")))

;; Assignment statement parser
(define (parse-assign-stmt tokens)
  (let ([id (parse-id tokens)])
    (if (equal? (cadr tokens) "=")
        (let ([expr (parse-expr (cddr tokens))])
          (if (equal? (cadr expr) ";")
              (list 'assign-stmt id (car expr))
              (error "Syntax error: Missing semicolon after assignment")))
        (error "Syntax error: Expected '=' in assignment"))))

;; [Include parse-if-stmt, parse-expr, parse-id, etc.]

;;Main Function
(define (parse-file filename)
  (parameterize ([current-line 1])
    (if (file-exists? filename)
        (let* ([input (file->string filename)]
               [tokens (tokenize input)])
          (with-handlers ([exn:fail? (λ (e) (displayln (exn-message e)))])
            (displayln (format "Tokens: ~a" tokens))
            (parse-program tokens)))
        (displayln (format "Error: File '~a' not found" filename)))))

;;Interactive Loop
(define (run-parser)
  (displayln "Enter filename (or 'quit' to exit):")
  (let loop ()
    (display "> ")
    (let ([input (read-line)])
      (unless (equal? input "quit")
        (parse-file input)
        (loop)))))


(run-parser)