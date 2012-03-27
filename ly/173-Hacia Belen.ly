﻿\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Hacia Belén va una burra"}}
  %poet = \markup\oldStyleNum"Traditional"
  %composer = \markup\oldStyleNum"Traditional"
  tagline = \markup { "Traditional Spanish Carol, from" \italic {cpdl.org} and \italic"pucpr.edu"}
}
\paper {
  %print-all-headers = ##t
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0.1)
       (stretchability . 0))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  bottom-margin = 0.125\in
  first-page-number = #173
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key g \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo "Allegro" 4 = 168
  \partial 4 d4 |
  g g g |
  g g b |
  d c b |
  a a8 g fis g |
  
  a b a g fis g |
  a4 d2~ |
  d2.~ |
  d2 a4 |
  
  %page2
  b c e |
  d2 c4 |
  b2( a4) |
  g2 d4 |
  g g g |
  
  g g b |
  d c b |
  a a8 g fis g |
  a b a g fis g |
  
  a4 d2~ |
  d2.~ |
  d2 a4 |
  b c e |
  
  %page3
  d2 c4 |
  b2( a4) |
  g2 \bar"|" \break
  
  \repeat volta 2 {
    b8 b |
    d4 d a |
    b2 b8 b |
    
    d4 d a |
    b2 g8 g |
    c4 c a |
    b2 g8 g |
    a4 a fis |
    
    g2
  }
}
sopWords = \lyricmode {
  \repeat unfold 22 {\skip1}
  ¡ah! __ ""
  \repeat unfold 29 {\skip1}
  ¡ah! __
}

altoMusic = \relative c' {
  d4 |
  d d d |
  d d d |
  d d d |
  fis fis8 e d e |
  
  fis g fis e d e |
  fis4 fis8 e d e |
  fis g fis e d e |
  fis2 fis4 |
  
  %page2
  d4 g g |
  g2 fis4 |
  g2( fis4) |
  d2 d4 |
  d4 d d |
  
  d d d |
  d d d |
  fis4 fis8 e d e |
  fis g fis e d e |
  
  fis4 fis8 e d e |
  fis g fis e d e |
  fis2 fis4 |
  d4 g g |
  
  %page3
  g2 fis4 |
  g2( fis4) |
  d2 g8 g |
  a4 a fis |
  g2 g8 g |
  
  a4 a fis |
  g2 d8 d |
  g4 g g |
  g2 d8 d |
  fis4 e d |
  
  d2
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Ha -- cia Be -- lén va u -- na bu -- rra rin rin
  yo me re -- men -- da -- ba yo me re -- men -- dé,
  yo me~e -- ché~un re -- mien -- do yo me lo qui -- té,
  car -- ga -- da de cho -- co -- la -- te.
  Lle -- va su cho -- co -- la -- te -- ra rin rin rin
  yo me re -- men -- da -- ba yo me re -- men -- rin,
  yo me~e -- ché~un re -- mien -- do yo me lo qui -- té,
  su mo -- li -- ni -- llo~y su~a -- na -- fre.
  Ma -- rí -- a Ma -- rí -- a
  ven a -- cá co -- rrien -- do
  que~el cho -- co -- la -- ti -- llo
  se lo~es -- tán co -- mien -- do.
  
  Ma -- rí -- a Ma -- rí -- a
  ven a -- cá co -- rrien -- do
  que~el cho -- co -- la -- ti -- llo
  se lo~es -- tán co -- mien -- do.
}
altoWordsII = {
  \set stanza = \markup{\dynamic"mp " "2. "}
  \lyricmode {
  %\markup\italic
    En el por -- tal de Be -- lén rin rin rin rin
    yo me re -- men -- da -- ba yo me re -- men -- dé,
    yo me~e -- ché~un re -- mien -- do yo me lo qui -- té,
    han en -- tra -- do los ra -- to -- nes.
    Y al bue -- no de San Jo -- sé rin rin rin
    yo me re -- men -- da -- ba yo me re -- men -- rin,
    yo me~e -- ché~un re -- mien -- do yo me lo qui -- té,
    Le han roi -- do los cal -- zo -- nes.
    Ma -- rí -- a Ma -- rí -- a
    ven a -- cá co -- rrien -- do
    que los cal -- zon -- ci -- llos
    los es -- tán roy -- en -- do.
    
    Ma -- rí -- a Ma -- rí -- a
    ven a -- cá co -- rrien -- do
    que los cal -- zon -- ci -- llos
    los es -- tán roy -- en -- do. 
  }
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  En el por -- tal de Be -- lén rin rin rin rin
  yo me re -- men -- da -- ba yo me re -- men -- dé,
  yo me~e -- ché~un re -- mien -- do yo me lo qui -- té,
  gi -- ta -- ni -- llos han en -- tra -- do
  Y al Ni -- ño que~es -- tá en la cu -- na rin
  yo me re -- men -- da -- ba yo me re -- men -- rin,
  yo me~e -- ché~un re -- mien -- do yo me lo qui -- té,
  los pa -- ña -- les la~han ro -- ba -- do.
  Ma -- rí -- a Ma -- rí -- a
  ve a -- cá vo -- lan -- do que
  los pa -- ña -- li -- tos los es -- tán lle -- van -- do.
  
  Ma -- rí -- a Ma -- rí -- a
  ve a -- cá vo -- lan -- do que
  los pa -- ña -- li -- tos los es -- tán lle -- van -- do.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  d4 |
  b b b |
  b b b |
  b a g |
  a a a |
  
  a a a |
  a a8 g fis g |
  a b a g fis g |
  a2 a4 |
  
  %page2
  g g c |
  b2 a 4 |
  d2( c4) |
  b2 d4 |
  b b b |
  
  b b b |
  b a g |
  a a a |
  a a a |
  
  a a8 g fis g |
  a b a g fis g |
  a2 a4 |
  g g c |
  
  %page3
  b2 a4 |
  d2( c4) |
  b2 d8 d |
  d4 d d |
  d2 d8 d |
  
  d4 d d |
  d2 b8 b |
  c4 c e |
  d2 b8 b |
  c4 c a |
  
  b2
}
tenorWords = \lyricmode {
  \repeat unfold 10 {\skip1}
  rin rin rin rin rin rin rin,
  \repeat unfold 29 {\skip1}
  rin rin rin rin rin rin 
  
}

bassMusic = \relative c {
  d4 |
  g g g |
  g g g |
  g fis g |
  d d d |
  
  d d d |
  d d d |
  d d d |
  d2 d4 |
  
  %page2
  g e c |
  d2 d4 |
  d2. |
  g2 d4 |
  g g g |
  
  g g g |
  g fis g |
  d d d |
  d d d |
  
  d d d |
  d d d |
  d2 d4 |
  g e c |
  
  %page3
  d2 d4 d2. |
  g2 g8 g |
  fis4 fis d |
  g2 g8 g |
  
  fis4 fis d |
  g2 g8 g |
  e4 e c |
  d2 d8 d |
  d4 d d |
  
  <g g,>2
}
bassWords = \lyricmode {
  \repeat unfold 16 {\skip1}
  rin rin rin
  rin rin rin
  rin,
  
  \repeat unfold 24 {\skip1}
  rin rin rin
  rin rin rin
  rin,
}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men"  \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 4)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
  }
}