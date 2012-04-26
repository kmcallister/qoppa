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

(define (m-vau static-env vau-operands)
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
    (define (wrap-primitive fun)
        (lambda (env operands)
            (apply fun (map (lambda (exp) (m-eval env exp)) operands))))
    (list
        (list 'vau m-vau)
        (list 'eval        (wrap-primitive m-eval))
        (list 'operate     (wrap-primitive m-operate))
        (list 'lookup      (wrap-primitive m-lookup))
        (list 'bool        (wrap-primitive (lambda (b t f) (if b t f))))
        (list 'eq?         (wrap-primitive eq?))
        (list 'null?       (wrap-primitive null?))
        (list 'symbol?     (wrap-primitive symbol?))
        (list 'pair?       (wrap-primitive pair?))
        (list 'cons        (wrap-primitive cons))
        (list 'car         (wrap-primitive car))
        (list 'cdr         (wrap-primitive cdr))
        (list 'set-car!    (wrap-primitive set-car!))
        (list 'set-cdr!    (wrap-primitive set-cdr!))
        (list '+           (wrap-primitive +))
        (list '*           (wrap-primitive *))
        (list '-           (wrap-primitive -))
        (list '/           (wrap-primitive /))
        (list '<=          (wrap-primitive <=))
        (list '=           (wrap-primitive =))
        (list 'error       (wrap-primitive error))
        (list 'display     (wrap-primitive display))
        (list 'read        (wrap-primitive read))
        (list 'eof-object? (wrap-primitive eof-object?))
        (list 'open-input-file (wrap-primitive open-input-file))))

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
