\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Girl I Left Behind Me"}}
  composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 100))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  f'8[ e] |
  d4 bes a4. g8 |
  a4 f d4. e8 |
  f4. f8 f[ g] a[ bes] |
  
  c2 a4 f'8[ e] |
  d4 c a4. g8 |
  a4 f d f |
  e g c, e |
  
  f2 f4 c'8[ bes] |
  a4 c d e |
  f c a c8[ bes] |
  a4 c d e |
  
  f2 e4 f8[ e] |
  d4 bes a g |
  a f d e8 f |
  e[ f] g4 c, e |
  f2 f4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	I’m lone -- some since I crossed the hill,
  And o’er the moor and val -- ley;
  Such heav -- y thoughts my heart do fill,
  Since part -- ing with my Sal -- ly.
  I seek no more the fine and gay,
  For each does but re -- mind me
  How swift the hours did pass a -- way,
  With the girl I’ve left be -- hind me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oh! ne’er shall I for -- get the night,
  The stars were bright a -- bove me,
  And gen -- tly lent their sil -- v’ry light,
  When first she vowed she loved me.
  But now I’m bound to Brigh -- ton camp,
  Kind heav’n, may fa -- vor find me,
  And send me safe -- ly back a -- gain
  To the girl I’ve left be -- hind me.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The bee shall hon -- ey taste no more,
  The dove be -- come a ran -- ger,
  The dash -- ing waves shall cease to roar,
  Ere she’s to me a stran -- ger;
  The vows we’ve reg -- is -- ter’d a -- bove
  Shall ev -- er cheer and bind me,
  In con -- stan -- cy to her I love,
  To the girl I’ve left be -- hind me.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  My mind her form shall still re -- tain,
  In sleep -- ing or in wak -- ing,
  Un -- til I see my love a -- gain,
  For whom my heart is break -- ing.
  If ev -- er I should see the day,
  When Mars shall have re -- signed me,
  For ev -- er -- more I’ll glad -- ly stay
  With the girl I’ve left be -- hind me.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  a'4 |
  f f f4. e8 |
  f4 c d4. c8 |
  c4. c8 c4 f 
  
  f2 f4 a4 
  f f f4. e8 
  f4 c d d 
  c8[ d] e4 c c |
  
  c2 c4 e |
  f f f g |
  a f f e |
  f f f g |
  
  a2 g4 a8[ c] |
  bes4 f f e |
  f c d c8 c |
  c4 e c c |
  c2 c4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
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
  c4 |
  bes d c4. bes8 |
  c4 a bes4. bes8 |
  a4. a8 a4 c |
  
  a2 c4 c |
  bes a c4. bes8 |
  c4 a bes bes |
  g bes g bes |
  
  a2 a4 g |
  f a bes c |
  c a c c |
  c a bes c |
  
  c2 c4 c8[ a] |
  bes4 d c bes |
  c a bes g8 a |
  g[ a] bes4 g bes |
  a2 a4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 |
  bes4 bes f4. f8 |
  f4 f f4. f8 |
  f4. f8 f4 f
  
  f2 f4 f
  f f f4. f8
  f4 f f f 
  c c e c 
  
  f2 f4 c |
  f f f f |
  f f f g |
  f f f f |
  
  f2 c4 f |
  f f f f |
  f f f c8 c |
  c4 c e c |
  f2 f4 \bar"|."
}
bassWords = \lyricmode {
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
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 180
    \set Staff.midiInstrument = "flute"
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
}
