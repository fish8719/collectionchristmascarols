﻿\version "2.14.2"
\include "../util.ly"
\version "2.14.2"
% Michael's utilities

#(define (splitUp chars pred)
       (define (helper chars pred lists)
         (if (null? chars) lists
             (let* (
                    (firstc (car chars))
                    (newlists (helper (cdr chars) pred lists))
                    (firstlist (car newlists)))
                   (if (or (null? firstlist)
                           (eq? (pred firstc) (pred (car firstlist))))
                       (cons (cons firstc firstlist) (cdr newlists))
                       (cons (list firstc) newlists))
              )
           ))
        (helper chars pred (list '()) )
)
           
#(define-markup-command (fakeCaps layout props str) (markup?)
 "Fake small-caps by turning (sequences of) lower-case letters into upper-case and changing the fontsize"
      (let*
           ((chars (string->list str))
            (lists (splitUp chars 
                (lambda (x) (and
                    (< (char->integer x) 128) 
                    (char-lower-case? x) ))))
            (strings (map
                       (lambda (s)
                               (if (char-lower-case? (car s))
                                   (markup #:fontsize -2 (list->string (map char-upcase s)))
                                   (list->string s) ) )
                       lists))
           (mu (markup #:override '(word-space . 0) (make-line-markup strings))) )
          (interpret-markup layout props mu)) )
 
%% UTF-8 utilities
#(define (list->utf-8 l) (apply string-append (map ly:wide-char->utf-8 l)))
           
#(define (utf-8->list str)
    (define (numBytes leader)
            (cond ((< leader #x80) 1)
                  ((< leader #xc0) (begin (stderr "programming-error: bad utf-8:~x\n" leader) 1))
                  ((< leader #xe0) 2)
                  ((< leader #xf0) 3)
                  ((< leader #xf8) 4)
                  (else (begin (stderr "programming-error: utf-8 too big:~x\n" leader) 1))))
     (define (helper start l n)
             (if (= n 0) start
                 (helper (+ (* start #x40) (modulo (car l) #x40)) (cdr l) (- n 1))))
     (define (utf-8->int l)
              (let* ((leader (car l))
                     (n (- (numBytes leader) 1))
                     (fac (/ #x80 (expt 2 n)))
                     (rest (cdr l))
                     (result (helper (modulo leader fac) rest n)))
                    result))
     (define (toListHelper lst chars)
                 (if (null? lst) (reverse chars)
                     (let* ((c (utf-8->int lst))
                            (n (numBytes (car lst)))
                            (t (list-tail lst n))
                            (newC (cons c chars)))
                        (toListHelper t newC))))
    (toListHelper (map char->integer (string->list str)) '() ))

%These are appropriate for Junicode, and other fonts.  Override as necessary
oldStyleZeroCode = ##xF730
smallCapsACode = ##xF761

%For Linux Libertine
oldStyleZeroCodeLL = ##xE01A
smallCapsACodeLL = ##xE051

#(define (change-char-helper aa test? offset)
    (if (string? aa) 
        (let* ((chars (string->list aa))
               (tosc (map (lambda (c)
                               (if (and (<= (char->integer c) 127) (test? c))
                                   (ly:wide-char->utf-8 (+ (char->integer c) offset))
                                   (string c) ) )
                           chars))
                (newStr (apply string-append tosc)))
             newStr)
         aa)
)

#(define (change-char-helper aa test? offset)
    (if (string? aa) 
        (let* ((chars (utf-8->list aa))
               (tosc (map (lambda (c)
                               (if (and (<= c 127) (test? (integer->char c)))
                                   (ly:wide-char->utf-8 (+ c offset))
                                   (if (and (<= c 255) (>= c 224))
                                       (ly:wide-char->utf-8 (+ c offset))
                                       (if (= c #x0153)
                                           (ly:wide-char->utf-8 #xF6FA)
                                           (ly:wide-char->utf-8 c) ) ) ) )
                           chars))
                (newStr (apply string-append tosc)))
             newStr)
         aa)
)

#(define (to-old-style str) (change-char-helper str char-numeric?
                                (- oldStyleZeroCode (char->integer #\0))))
                                
#(define (to-small-caps str) (change-char-helper str char-lower-case?
                                (- smallCapsACode (char->integer #\a))))

#(define-markup-command (realCaps layout props str) (markup?)
    "Real small capitals"
    (interpret-markup layout props (to-small-caps str)))
    
#(define-markup-command (oldStyleNum layout props str) (markup?)
    "Old-style numerals"
    (interpret-markup layout props (to-old-style str)))

#(define-markup-command (oldStylePageNum layout props str) (markup?)
    "Old-style Page numbers"
    (interpret-markup layout props (to-old-style (number->string (chain-assoc-get 'page:page-number props -1)))))
    
#(define-markup-command (smallCapsOldStyle layout props str) (markup?)
    "Real small caps and old-style numerals"
    (interpret-markup layout props (to-old-style (to-small-caps str))))
    
#(define-markup-command (concat layout props markups) (markup-list?)
    "Concatenate markups with no spaces"
    (interpret-markup layout (cons '((word-space . 0)) props) 
            (make-line-markup markups)))
%%%
#(define-public (bar-number-print grob)
  "Print function for making oldStyle numbers.  Useful for BarNumber, for example"
  (let*
      ((text (ly:grob-property grob 'text))
       (layout (ly:grob-layout grob))
       (defs (ly:output-def-lookup layout 'text-font-defaults))
       (props (ly:grob-alist-chain grob defs)))

    (ly:text-interface::interpret-markup layout
					 props
					 (if (string? text)
					     (markup #:oldStyleNum text)
					     text))))
              
%%%
% Utilities for Baskerville 1757 ligatures
% like string-split, but with a substring rather than a char
#(define (string-split-sub str sub)
        (let* ((idx (string-contains str sub)))
              (if idx
                  (let* ((len1 (string-length sub))
                         (first (substring str 0 idx))
                         (rest (substring str (+ idx len1)))
                         (lrest (string-split-sub rest sub)))
                       (cons first lrest))
                  (list str)
)))

% takes a string, returns a list
#(define (replace-subs str from to)
         (if (string? str)
             (list-join (string-split-sub str from) to)
             (list str)))
               
%list -> list
#(define (replace-subr strings subs . rest)
        (let* ((fn (if(null? rest) (lambda (x) x) (car rest) )))
            (if (null? subs)
                strings
                (replace-subr (apply append (map
                          (lambda (str) (replace-subs str (car subs) (fn (cadr subs)) ))
                          strings ))
                    (cddr subs) fn))))

#(define (make-lig-list str . rest)
        (apply replace-subr (cons (list str) (cons (list "ffi" "ﬃ" "ffl" "ﬄ" "fi" "ﬁ" "fl" "ﬂ" "ff" "ﬀ" ) rest))))
       
#(define (make-blig-markup-fn ligFont)
        (lambda (s)
        (define (imagstep x) (* (log x) (/ 6 (log 2))))
        (let ((step (imagstep (/ 11 12))))
            (markup #:fontsize step #:override `(font-name . ,ligFont) s))))

#(define (make-bv-lig-list ligFont str)
        (apply replace-subr (cons (list str) (cons (list "ﬃ" "I" "ﬄ" "L" "ﬁ" "i" "ﬂ" "l" "ﬀ" "f" ) (list
                    (make-blig-markup-fn ligFont ))))))
                   
#(define (make-bv-hlig-list ligFont l)
        (apply replace-subr (cons l (cons (list "ct" "C" "st" "S" ) (list
                    (make-blig-markup-fn ligFont) )))))

#(define (add-ligs str)
        (apply string-append (make-lig-list str)))

       
doHlig = ##t

#(define-markup-command (addBLigs layout props str) (markup?) 
        "Add Baskerville 1757 ligatures, including st and ct, to a string"
        (let* ((str1 (add-ligs str))
               (shape (chain-assoc-get 'font-shape props #f))
               (isItalic (eqv? shape 'italic))
               (ligFont (if isItalic "Baskerville1757Ligatures Italic" "Baskerville1757Ligatures"))
               (markups1 (make-bv-lig-list ligFont str1))
               (markups (if doHlig (make-bv-hlig-list ligFont markups1) markups1))
               (result (markup #:override '(word-space . 0) (make-line-markup markups ))))
       (interpret-markup layout props result)))
       
#(define-markup-list-command (addBLigsList layout props strings) (markup-list?) 
        "Add Baskerville 1757 ligatures, including st and ct, to a list of strings"
        (interpret-markup-list layout props 
            (map (lambda (x) (markup #:addBLigs x)) strings)))
       
#(define-public (add-bligs-print grob)
  "Print function for adding Baskerville ligatures"
  (let*
      ((text (ly:grob-property grob 'text))
       (layout (ly:grob-layout grob))
       (defs (ly:output-def-lookup layout 'text-font-defaults))
       (props (ly:grob-alist-chain grob defs)))

    (ly:text-interface::interpret-markup layout
					 props
					 (if (string? text)
					     (markup #:addBLigs text)
					     text))))