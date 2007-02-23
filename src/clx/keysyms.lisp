;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:YES -*-

;;; Define lisp character to keysym mappings

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
#+cmu
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/clx/keysyms.lisp,v 1.4 2003/04/11 12:07:30 pmai Exp $")

(in-package :xlib)

(define-keysym-set :latin-1	(keysym 0 0) (keysym 0 255))
(define-keysym-set :latin-2	(keysym 1 0) (keysym 1 255))
(define-keysym-set :latin-3	(keysym 2 0) (keysym 2 255))
(define-keysym-set :latin-4	(keysym 3 0) (keysym 3 255))
(define-keysym-set :kana	(keysym 4 0) (keysym 4 255))
(define-keysym-set :arabic	(keysym 5 0) (keysym 5 255))
(define-keysym-set :cyrillic	(keysym 6 0) (keysym 6 255))
(define-keysym-set :greek	(keysym 7 0) (keysym 7 255))
(define-keysym-set :tech	(keysym 8 0) (keysym 8 255))
(define-keysym-set :special	(keysym 9 0) (keysym 9 255))
(define-keysym-set :publish	(keysym 10 0) (keysym 10 255))
(define-keysym-set :apl		(keysym 11 0) (keysym 11 255))
(define-keysym-set :hebrew	(keysym 12 0) (keysym 12 255))
(define-keysym-set :keyboard	(keysym 255 0) (keysym 255 255))

(define-keysym :character-set-switch character-set-switch-keysym)
(define-keysym :left-shift left-shift-keysym)
(define-keysym :right-shift right-shift-keysym)
(define-keysym :left-control left-control-keysym)
(define-keysym :right-control right-control-keysym)
(define-keysym :caps-lock caps-lock-keysym)
(define-keysym :shift-lock shift-lock-keysym)
(define-keysym :left-meta left-meta-keysym)
(define-keysym :right-meta right-meta-keysym)
(define-keysym :left-alt left-alt-keysym)
(define-keysym :right-alt right-alt-keysym)
(define-keysym :left-super left-super-keysym)
(define-keysym :right-super right-super-keysym)
(define-keysym :left-hyper left-hyper-keysym)
(define-keysym :right-hyper right-hyper-keysym)

(define-keysym #\space 032)
(define-keysym #\! 033)
(define-keysym #\" 034)
(define-keysym #\# 035)
(define-keysym #\$ 036)
(define-keysym #\% 037)
(define-keysym #\& 038)
(define-keysym #\' 039)
(define-keysym #\( 040)
(define-keysym #\) 041)
(define-keysym #\* 042)
(define-keysym #\+ 043)
(define-keysym #\, 044)
(define-keysym #\- 045)
(define-keysym #\. 046)
(define-keysym #\/ 047)
(define-keysym #\0 048)
(define-keysym #\1 049)
(define-keysym #\2 050)
(define-keysym #\3 051)
(define-keysym #\4 052)
(define-keysym #\5 053)
(define-keysym #\6 054)
(define-keysym #\7 055)
(define-keysym #\8 056)
(define-keysym #\9 057)
(define-keysym #\: 058)
(define-keysym #\; 059)
(define-keysym #\< 060)
(define-keysym #\= 061)
(define-keysym #\> 062)
(define-keysym #\? 063)
(define-keysym #\@ 064)
(define-keysym #\A 065 :lowercase 097)
(define-keysym #\B 066 :lowercase 098)
(define-keysym #\C 067 :lowercase 099)
(define-keysym #\D 068 :lowercase 100)
(define-keysym #\E 069 :lowercase 101)
(define-keysym #\F 070 :lowercase 102)
(define-keysym #\G 071 :lowercase 103)
(define-keysym #\H 072 :lowercase 104)
(define-keysym #\I 073 :lowercase 105)
(define-keysym #\J 074 :lowercase 106)
(define-keysym #\K 075 :lowercase 107)
(define-keysym #\L 076 :lowercase 108)
(define-keysym #\M 077 :lowercase 109)
(define-keysym #\N 078 :lowercase 110)
(define-keysym #\O 079 :lowercase 111)
(define-keysym #\P 080 :lowercase 112)
(define-keysym #\Q 081 :lowercase 113)
(define-keysym #\R 082 :lowercase 114)
(define-keysym #\S 083 :lowercase 115)
(define-keysym #\T 084 :lowercase 116)
(define-keysym #\U 085 :lowercase 117)
(define-keysym #\V 086 :lowercase 118)
(define-keysym #\W 087 :lowercase 119)
(define-keysym #\X 088 :lowercase 120)
(define-keysym #\Y 089 :lowercase 121)
(define-keysym #\Z 090 :lowercase 122)
(define-keysym #\[ 091)
(define-keysym #\\ 092)
(define-keysym #\] 093)
(define-keysym #\^ 094)
(define-keysym #\_ 095)
(define-keysym #\` 096)
(define-keysym #\a 097)
(define-keysym #\b 098)
(define-keysym #\c 099)
(define-keysym #\d 100)
(define-keysym #\e 101)
(define-keysym #\f 102)
(define-keysym #\g 103)
(define-keysym #\h 104)
(define-keysym #\i 105)
(define-keysym #\j 106)
(define-keysym #\k 107)
(define-keysym #\l 108)
(define-keysym #\m 109)
(define-keysym #\n 110)
(define-keysym #\o 111)
(define-keysym #\p 112)
(define-keysym #\q 113)
(define-keysym #\r 114)
(define-keysym #\s 115)
(define-keysym #\t 116)
(define-keysym #\u 117)
(define-keysym #\v 118)
(define-keysym #\w 119)
(define-keysym #\x 120)
(define-keysym #\y 121)
(define-keysym #\z 122)
(define-keysym #\{ 123)
(define-keysym #\| 124)
(define-keysym #\} 125)
(define-keysym #\~ 126)

(progn   ;; Semi-standard characters
  (define-keysym #\rubout (keysym 255 255))	; :tty
  (define-keysym #\tab (keysym 255 009))	; :tty
  (define-keysym #\linefeed (keysym 255 010))	; :tty
  (define-keysym #\page (keysym 009 227))	; :special
  (define-keysym #\return (keysym 255 013))	; :tty
  (define-keysym #\backspace (keysym 255 008))	; :tty
  )


