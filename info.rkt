#lang info

;; Basic package information
(define collection "ramsay-sicp-solutions")
(define version "1.0")
(define pkg-desc "Ramsay's solutions and notes for SICP exercises.")

;; Dependencies - this is like requirements.txt
(define deps '("base"           ; Always needed
               "rackunit-lib"   ; For testing
               "sicp"
               ))

;; Build dependencies (only needed during development)
(define build-deps '("scribble-lib"  ; For documentation
                     "racket-doc"    ; For documentation
                     "rackunit-lib"  ; For testing
                     ))

;; Test dependencies
(define test-deps '("rackunit-lib"))

;; Which files to include when installing
(define pkg-authors '("Ramsay Leung(ramsayleung@gmail.com"))

