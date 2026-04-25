(in-package #:tagless-compiler)

;; Blub (c-like language)
(defparameter *blub* (make-interpreter :on-unknown :passthrough))

;; Blub pretty printer
(defparameter *blub-print* (make-interpreter :on-unknown :passthrough))

;; Define individual passes within the blub language
(defparameter *blub-0* (make-interpreter :on-unknown :passthrough))

;; How to store intermediate data to carry between passes? Like symbol table,
;; and the types of each variable. I guess the goal is to maybe try and keep
;; as much of the information local as possible.
;;
;; What if want to treat code differently depending on the outermost tag?

;; Language features
;;
;; declare variables with a type, variable assignment
;;  - int, float, double, boolean, pointer, arrays, byte (char)
;;  - everything stack allocated by default with alloc, read with load, set with store, since you need to be able to take their address
;;    - temporaries don't need an alloc, since you can't get a pointer to them
;; if, for, while, switch
;; functions
;; can declare global variables
;; standard arithmetic operators, comparisons, logic, bitwise
;; "block" (in curly braces)
;; function pointers
;; struct
;;  - note that in QBE, passing a struct using the aggregate type class is passing by value, while passing with type :l means passing the pointer itself (even though technically it is always a pointer)
;;  - so QBE deals with the C ABI for you when passing structs around
;; varargs
;; some way of accessing argv/argc
;; standard library functions for printing, etc
;;
;;
;; Compiler passes (in no particular order)
;; - typecheck standard operators, function calls, pointers, etc
;; - resolve all struct definitions, determine total size, plus size and offset of each field
;; - cps transformation from nested statements into SSA



