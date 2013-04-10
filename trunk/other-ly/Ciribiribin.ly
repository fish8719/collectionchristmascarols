\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Ciribiribin"}}
  composer = \markup\oldStyleNum"Alberto Pestalozza (1858–1934)"
  %poet = \markup\oldStyleNum"Italian lyrics, Carlo Tiochet (1863–1912)"
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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##t
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
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 2
  c'4 a |
  g2 f4 |
  e2 f4 |
  d8[ e] c2~ |
  c4 f g |
  a2 c4 |
  d2 f4 |
  e2.~ |
  e8 b\rest e4 g |
  d2 f4 |
  
  e2 d4 |
  e8[ d] g,2~ |
  g8 b\rest g4 a |
  bes2 c4 |
  d2 e4 |
  c2.~ |
  c8 b\rest c4 a |
  g2 f4 |
  e2 f4 |
  
  d8[ e] c2~ |
  c4 c' d |
  ees2 d4 |
  ees2 d4 |
  d2.~ |
  d8 b\rest\fermata g4 a |
  bes2 a4 |
  g2 d'4 |
  c8[ a] f2~ |
  
  f8 b\rest f4 g |
  a( c) a |
  g2\fermata c4 |
  f,2 b4\rest |
  b\rest c8 b bes a |
  c b\rest c b bes a |
  c b\rest c b bes a |
  
  c2.~ |
  c8 b\rest \bar"||"
  c b bes a |
  c2 a4 |
  g2 f4 |
  c2 f4 |
  g2 f4 |
  c2 a'4 |
  g2 f4 |
  
  bes2.~ |
  bes8 bes\rest d des c bes |
  ees2 d4 |
  bes2 a4 |
  g2 g4 |
  a2 g4 |
  d'2 f4 |
  e2 d4 |
  d2.~ |
  d8 b\rest c b bes a |
  c2 a4 |
  g2 f4 |
  c2 f4 |
  g2 f4 |
  c2 a'4 |
  g2 f4 |
  d'2.~ |
  d8 b\rest e d cis d |
  
  f2.~ |
  f8 b,\rest e d cis d |
  f2.~ |
  f8 b,\rest a g fis g |
  bes2 e,4 |
  a2\fermata g4 |
  f2.~ |
  f8 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = \markup\dynamic"p "
	I am wait -- ing here for you love
  As the eve -- ning breez -- es blow
  Watch -- ing shad -- ows of the riv -- er
  As they flit both to and fro.
  I have come to see the love -- light
  danc -- ing in your eyes of blue,
  And to hear you soft -- ly whis -- per
  that to me you’ll e’er be true.
  
  Ci -- ri -- bi -- ri -- bin,
  Ci -- ri -- bi -- ri -- bin,
  Ci -- ri -- bi -- ri -- bin,
  
  \set stanza = \markup\dynamic"mf "
  Ci -- ri -- bi -- ri -- bin,
  the moon looks down up -- on our hap -- pi -- ness se -- rene,
  Ci -- ri -- bi -- ri -- bin,
  the stars bow down be -- fore thee, O my ra -- diant queen.
  
  Ci -- ri -- bi -- ri -- bin,
  more love than mine for thee the world has nev -- er seen.
  Ci -- ri -- bi -- ri -- bin,
  Ci -- ri -- bi -- ri -- bin,
  Ci -- ri -- bi -- ri -- bin,
  my ra -- diant queen.
}

sopWordsII = \lyricmode {
  %\set stanza = #"2. "
  
}

sopWordsIII = \lyricmode {
  %\set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  %\set stanza = #"4. "
}

sopWordsV = \lyricmode {
  %\set stanza = #"5. "
}

altoMusic = \relative c' {
  
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
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  
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
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
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
