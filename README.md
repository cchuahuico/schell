schell
======

schell is an interpreter written in Haskell for a subset of the Scheme programming language. Special forms implemented include:

- variable reference
- constant literal
- quotation
- conditional
- assignment
- definition
- let syntactic sugar
- procedure application

Examples
========

Here is a Y Combinator for the factorial function:
```
(define Y
 (lambda (X)
  ((lambda (procedure)
     (X (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     (X (lambda (arg) ((procedure procedure) arg)))))))

(define F*
 (lambda (func-arg)
  (lambda (n)
    (if (= n 0)
        1
        (* n (func-arg (- n 1)))))))
        
(define fact (Y F*))
(fact 10) ; 3628800
```

An example of a closure:
```
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define c (make-counter))

(c) ; 1
(c) ; 2
(c) ; 3
```

Usage
=====

There are two main ways to use the interpreter. 

1) Invoking the executable directly from the shell will start the REPL

```
schell>
schell> (define var 5)
#<void>
schell> var
5
schell> :l /home/username/testfile.scm
#t
#<procedure>
"hello"
schell> :q
```

2) Interpret a file passed as an argument to the executable

``` 
./schell /home/username/testfile.scm
```
```
#t
#<procedure>
"hello"
```

Build
=====

To build the interpreter, run

```
cabal clean
cabal configure
cabal build
```

The executable can be found in ```{PROJECTROOT}/dist/schell/schell```

Test
====

To run the tests, execute ```./runtests.sh```
