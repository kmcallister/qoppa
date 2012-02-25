(define (bind param val) (cond
    ((and (null? param) (null? val))
        '())
    ((eq? param '_)
        '())
    ((symbol? param)
        (list (list param val)))
    ((and (pair? param) (pair? val))
        (append
            (bind (car param) (car val))
            (bind (cdr param) (cdr val))))
    (else
        (error "can't bind" param val))))

(define (m-lookup name env)
    (if (null? env)
        (error "could not find" name)
        (let ((binding (assq name (car env))))
            (if binding
                binding
                (m-lookup name (cdr env))))))

(define (m-eval env exp) (cond
    ((symbol? exp)
        (cadr (m-lookup exp env)))
    ((pair? exp)
        (m-operate env (m-eval env (car exp)) (cdr exp)))
    (else
        exp)))

(define (m-operate env operative operands)
    (operative env operands))

(define (make-operative static-env vau-operands)
    (let ((params    (car   vau-operands))
          (env-param (cadr  vau-operands))
          (body      (caddr vau-operands)))

        (lambda (dynamic-env operands)
            (m-eval
                (cons
                    (bind
                        (cons env-param   params)
                        (cons dynamic-env operands))
                    static-env)
                body))))

(define (make-global-frame)
    (define (wrap-prim pair)
        (list (car pair) (lambda (env operands)
            (apply (cadr pair)
                (map (lambda (exp) (m-eval env exp)) operands)))))
    (cons
        (list 'vau make-operative)
        (map wrap-prim (list
            (list 'lookup   m-lookup)
            (list 'eval     m-eval)
            (list 'operate  m-operate)
            (list 'bool     (lambda (b t f) (if b t f)))
            (list 'eq?      eq?)
            (list 'null?    null?)
            (list 'symbol?  symbol?)
            (list 'pair?    pair?)
            (list 'cons     cons)
            (list 'car      car)
            (list 'cdr      cdr)
            (list 'set-car! set-car!)
            (list 'set-cdr! set-cdr!)
            (list 'error    error)
            (list 'display  display)
            (list '+  +)
            (list '*  *)
            (list '-  -)
            (list '/  /)
            (list '<= <=)
            (list '=  =)
            (list 'open-input-file open-input-file)
            (list 'read            read)
            (list 'eof-object?     eof-object?)))))

(define global-env (list (make-global-frame)))

(define (execute-file filename)
    (let ((stream (open-input-file filename)))
        (define (loop)
            (let ((exp (read stream)))
                (if (eof-object? exp)
                    'done
                    (begin
                        (display exp) (display "\n")
                        (m-eval global-env exp)
                        (loop)))))
        (loop)))

(execute-file "prelude.qop")
