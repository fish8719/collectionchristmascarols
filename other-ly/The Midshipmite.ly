\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Midshipmite"}}
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  composer = \markup\oldStyleNum"Stephen Adams (1841–1913)"
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
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #72
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
  \key c \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 \slurDotted
  g'8( g) |
  c4 c b b |
  a a g b\rest |
  e,8 d c d e4 a |
  g2 b4\rest g |
  c c b b |
  
  a a g e8[ f] |
  g4 e8 d c4 a'8( a) |
  \tieDotted
  << {g4. s8} {s4 \teeny e8~ \normalsize e} >> c4 b'\rest |
  \tieSolid
  b8 b a g d'4 d |
  
  g,2 b\rest |
  g8 a a a a4 b8( a) |
  g4. g8 g4 g8( g) |
  a4 a a b8[ a] |
  g4 g g a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 b'\rest |
  b8 b b b b4 fis |
  \slurSolid
  b2( cis) |
  d4-- e-- d-- b-- |
  
  \acciaccatura {a16[ b]} a2. g4 |
  \time 3/4
  g2.~^\markup\italic"rall." |
  g4 b4. a8 |
  g2^\markup\italic"a tempo" g4 |
  g8 b\rest b4 a |
  g2 g4 |
  g4 b2\rest |
  g4 e g |
  c( g) c |
  
  d2.~^\markup\italic"rall." |
  d4 e4.\fermata d8 |
  c2 g4 |
  e8 b'\rest b4 a |
  g2 f4 |
  d8 b'\rest e,4 g |
  c g c |
  d2 c4 |
  c2.~ |
  c4 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	’Twas in fif -- ty -- five, on~a win -- ter’s night,
  Cheer -- i -- ly my lads yo -- ho!
  We’d got the Roo -- shan lines in sight,
  When _ up comes a lit -- tle __ _ Mid -- _ ship -- mite,
  Cheer -- i -- ly my lads yo -- ho!
  
  “Who -- ’ll go a -- shore to -- _ night,” says he,
  “An’ _ spike their guns a -- _ long wi’ me?”
  “Why, _ bless ’ee, __ _ sir, come a -- long!” says we,
  
  
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  We _ launch’d for~the cutter and shoved her out,
  Cheer -- i -- ly my lads yo -- ho!
  The lub -- bers might ha’ heard us shout,
  As __ _ the Mid -- dy cried, “Now, my lads, put a -- bout.”
  Cheer -- i -- ly my lads yo -- ho!
  “We made for the guns, an’ we ram’d them tight,
  But the mus -- ket shots came _ left and right,
  An’ _ down drops the poor lit -- tle Mid -- ship -- mite,
  
  \unset ignoreMelismata
  Cheer -- i -- ly my lads yo -- ho! __
  Cheer -- i -- ly my lads yo -- ho! __
  With a long, long pull, An’ a strong, strong pull,
  Gai -- ly boys, make her go! __
  An’ we’ll drink to -- night
  To the Mid -- ship -- mite,
  Sing -- ing cheer -- i -- ly, lads, yo -- ho! __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  “I’m _ done for now; good -- bye!” says he,
  Stead -- i -- ly, my lads, yo -- ho!
  “You make for~the boat, never mind for me!”
  “We’ll _ take ’ee __ _ back, sir or die,” "" says we,
  Cheer -- i -- ly, my lads, yo -- ho!
  “So we hoist -- ed him~in, in a terri -- ble plight,
  An’ we pull’d ev’ry man with _ all his might,
  An’ _ saved the __ _ poor lit -- tle Mid -- ship -- mite,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  \slurDotted
  e8( e) |
  e4 e e e |
  f f e s |
  e8 d c d e4 f |
  e2 s4 e |
  e e e d |
  
  c f e e8[ f] |
  g4 e8 d c4 c8( c) |
  \tieDotted
  << {c4. s8} {s4 \teeny c8~ \normalsize c8} >> c4 s |
  \tieSolid
  d8 d d d fis4 fis |
  
  g2 s |
  e8 e f f f4 f8( f) |
  f4. f8 f4 f8( f) |
  f4 f f f |
  f f f a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 s |
  e8 e e e dis4 dis |
  \slurSolid
  e2( g) |
  g4 g g g |
  
  fis2. g4 |
  \time 3/4
  d4( e f)~ |
  f f4. f8 |
  f2 f4 |
  f8 s8 f4 f |
  e2 e4 |
  e s s |
  e e e |
  e2 e4 |
  
  f2.~ |
  f4 g4. f8 |
  e2 e4 c8 s e4 e |
  d2 d4 |
  b8 s c4 e |
  e e g |
  f2 e4 |
  e2.~ |
  e4 s \bar"|."
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
  \partial 4
  \slurDotted
  c8( c) |
  a4 a g g |
  f c' c s |
  e,8 d c d e4 c' |
  c2 s4 c |
  a a gis gis |
  
  f c' c e,8[ f] |
  g4 e8 d c4 f8( f) |
  \tieDotted
  << {e4. s8} {s4 \teeny g8~ \normalsize g} >> e4 s |
  \tieSolid
  g8 g c b a4 c |
  
  b2 s |
  c8 c c c c4 d8( c) |
  b4. b8 b4 b8( b) |
  c4 c c c |
  b b b a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 s |
  g'8 g g g fis4 a |
  \slurSolid
  g2( bes) |
  b?4 b b d |
  
  c2. b4 |
  \time 3/4
  b4( c d)~ |
  d c4. c8 |
  b2 b4 |
  b8 s d4 b |
  c2 c4 |
  c s s |
  b g b |
  c2 g4 |
  
  b2.~ |
  b4 b4. b8 |
  c2 c4 |
  g8 s g4 c |
  b2 b4 |
  g8 s g4 g |
  g c c |
  b2 g4 |
  g2.~ |
  g4 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  \slurDotted
  c,8( c) |
  a'4 a, e' e |
  f f c d\rest |
  e8 d c d e4 c |
  c2 d4\rest c |
  a' a, e' e |
  
  f f c e8[ f] |
  g4 e8 d c4 f,8( f) |
  \tieDotted
  << {c'4. s8} {s4 \teeny c8~ \normalsize c} >> c4 d\rest |
  \tieSolid
  d8 d d d d4 d |
  
  g2 d\rest |
  g8 g g g g4 g8( g) |
  g4. g8 g4 g8( g) |
  g4 g g g |
  g g g a8[ b] |
  
  c4 e,8( f) g4 a8 a |
  g4. e8 c4 d\rest |
  b8 b b b b4 b |
  \slurSolid
  e2( ees) |
  d4 d d d |
  d2. g4 |
  \time 3/4
  g2.~ |
  g4 g4. g8 |
  g2 g4 |
  g8 d\rest g4 g |
  c,2 c4 |
  c d2\rest |
  e4 e e |
  c2 c4 |
  g'2.~ |
  g4 g4.\fermata g8 |
  c,2 c4 |
  c8 d\rest c4 c |
  g'2 g4 |
  g,8 d'\rest g,4 g |
  c c e |
  g2 g4 |
  c,2.~ |
  c4 d\rest \bar"|."
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
    \tempo 4 = 130
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

