(load "recscm.scm")
(load "records.scm")
(load "tree.scm")
;A partial implementation of the interpreter discussed in the class
;Covers identifier, true/false, plusExpr and IfExpr
(define-record normal-env (ids vals env))

(define-record closure (formals body env))

(define empty-env '())

(define extend-env
  (lambda (id val env)
    (cons (cons id (box val)) env)))

(define apply-env
  (lambda (env id)
    (if (or (null? env) (null? id))
        null
        (let ((key (cadaar env))
              (val (cdar env))
              (env-prime (cdr env)))
          (if (eq? id key) val (apply-env env-prime id))))))

(define extend-env-list
  (lambda (ids vals env)
    (if (null? ids)
        env
        (extend-env-list
         (cdr ids)
         (cdr vals)
         (extend-env (car ids) (car vals) env)))))

(define get-ids (lambda (l)
                  (if (null? l) '() (cons (car (cdr (cdr (car l)))) (get-ids (cdr l))))))

(define get-exprs (lambda (l)
                    (if (null? l) '() (cons (car (cdddar l)) (get-exprs (cdr l))))))

(define changevar (lambda (id val env)
                 (set-box! (apply-env env (cadr id)) val)))

(define ret-recenv (lambda (env ids)
                     (if (null? ids) env
                         (let*
                             ((id (car ids))
                             (exp (apply-env env (cadr id)))
                             (val (eval-Expression (unbox exp) env)))
                           (changevar id val env)
                         (ret-recenv env (cdr ids)))
                         )
))
                 
;this is actual sample
(define eval-Expression
	(lambda (Expression env) 
		(record-case Expression
                (IntegerLiteral (Token) (string->number Token))
		(TrueLiteral (Token) #t)
		(FalseLiteral (Token) #f)
		(PlusExpression (Token1 Token2 Expression1 Expression2 Token3)
			(+ (eval-Expression Expression1 env) (eval-Expression Expression2 env)))
		(IfExpression (Token1 Token2 Expression1 Expression2 Expression3 Token3)
			(if (eval-Expression Expression1 env) (eval-Expression Expression2 env) (eval-Expression Expression3 env)))
                (Identifier (Token) (eval-Expression (unbox (apply-env env Token)) env))
                ;procedure
                (ProcedureExp (Token1 Token2 Token3
                                      List Token4 Expression Token5)
                              (make-closure List Expression env))
                ;app
                (Application (Token1 Expression List Token2)
                             (let*
                                 ((clo (eval-Expression Expression env))
                                  (ids (closure->formals clo))
                                  ;(vals (map (lambda (Exp)
                                               ;(eval-Expression Exp env))
                                             ;List))
                                  (static-env (closure->env clo))
                                  (new-env
                                   (extend-env-list ids List static-env))
                               (body (closure->body clo)))
                               (eval-Expression body new-env)))
                ;let
                (LetExpression (Token1 Token2 Token3
                                       List Token4 Expression Token5)
                               (let* ((ids (get-ids List))
                                      (exps (get-exprs List))
                                      ;(vals (map (lambda (Expression)
                                       ;            (eval-Expression Expression env))
                                        ;         exps))
                                      (new-env (extend-env-list ids exps env)))
                                 (eval-Expression Expression new-env)))
                ;let-rec
                (RecExpression (Token1 Token2 Token3
                                       List Token4 Expression Token5)
                               (let* ((ids (get-ids List))
                                      (exps (get-exprs List))
                                      (new-funenv (extend-env-list ids exps env))
                                      (new-env (ret-recenv new-funenv ids)))
                                 
                               (eval-Expression
                                Expression new-env)))
                ;Assignment
                (Assignment (Token1 Token2 Identifier Expression Token3)
                            (changevar Identifier (eval-Expression Expression env) env))
		(else (error 'eval-Expression "Expression not found")))))
(define run
(lambda ()
	(record-case root
		(Goal (Expression Token)
		  (eval-Expression Expression empty-env))
		 (else (error 'run "Goal not found")))))
;till here
(run)
