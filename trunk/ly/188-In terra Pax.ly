\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"In Terra Pax"}}
  poet = \markup\oldStyleNum"Mrs. Alderson"
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823-1876)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  first-page-number = #188
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
  \key f \major
  \time 6/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  c4^\mf f a c2 a4^\markup\italic"cresc." |
  a( g) a f2. \bar "||"
  f4^\p f f c'( a) d |
  d2.(^\> g,)\! \bar "||"
  
  g4^\markup\italic"cresc." g a bes( c) d |
  d( c) a g2. \bar "||"
  g4^\markup\italic"dim." g a bes( c) d |
  \partial 4*5 d(^\> c)\! a bes2 \bar "||"
  
  \partial 4 c4^\f |
  a2 a4 a( g) f |
  \partial 4*5 c'2.~ c2 \bar "||"
  \partial 4 c,4 |
  c2 a'4 a(^\> g)\! f |
  \partial 4*5 f( e) d c2 \bar "||" \break
  
  \partial 4 b'4\rest |
  b1.\rest |
  b2.\rest b2\rest c,4^\p |
  f2. d |
  f~ f2 c4 |
  c2.~ c4( d) e |
  
  f2( g4 a2 bes4 |
  c2.~ c~ |
  c~ c |
  a2) f4 a2 d4 |
  
  c2 bes4 a2 g4 |
  f2.~ f |
  f~ f~ |
  f~ f |
  bes1.\rest |
  a2.~ a2.\fermata \bar "||" \break
  
  % verse 4
  c,4 f a c2 a4 |
  \partial 4*5 a( g) a f2 \bar "||"
  \partial 4 f4 |
  f2 f4 c'( a) d |
  d2.(^> g,) \bar "||"
  
  g4 g a bes( c) d |
  d( c) a g2. \bar "||"
  g4^\markup\italic"dim." g^> a bes( c) d |
  \partial 4*5 d( c) a bes2 \bar "||"
  
  \partial 4 c4^\f |
  a2 a4 a( g) f |
  \partial 4*5 c'2. c2 \bar "||"
  \partial 4 c,4^\pp |
  c2. c |
  c a' |
  
  g2^> f4^\markup\italic"rall." e2 d4 |
  \partial 4*5 c2.~ c2 \bar "||"  \pageBreak
  \partial 4 b'4\rest |
  b1.\rest |
  b2.\rest b2\rest c,4^\f |
  f2. d |
  f~ f2 c4~ |
  c2.~^\markup\italic"cresc." c4 d e |
  
  f2( g4) a2( bes4) |
  c2.~ c~ |
  c~ c2 bes4 |
  a2 f4^\ff a2 d4 |
  c2 c4 d2 e4 |
  f2 bes,4\rest bes2.\rest |
  
  bes2\rest f'4 e2 d4 |
  c2. a |
  g c |
  a2 bes4\rest bes2.\rest |
  
  bes2\rest f'4 e2 d4 |
  c2. a |
  g c |
  a2 e'4\rest e2.\rest |
  
  e2\rest f4 e2 d4 |
  c2.~ c |
  f~ f |
  a,~ a( |
  g)~ g2 f4 |
  f1. \bar "|."
}
sopWords = {
  \lyricmode {
    \repeat unfold 44 {\skip 1}
    \repeat unfold 7 {\skip 1}
    \markup\italic Pax __
  }
  \set stanza = \markup\dynamic"mf"
  \lyricmode {
    \markup\italic In \markup\italic ter -- \markup\italic ra \markup\italic Pax \markup\italic ho -- \markup\italic mi -- \markup\italic ni -- \markup\italic bus, __ \markup\italic Pax __
  }
  \set stanza = \markup\dynamic"pp"
  \lyricmode {
    \markup\italic Pax.”
    
    \repeat unfold 47 {\skip 1}
    \repeat unfold 36 {\skip 1}
    \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, __
  }
}

altoMusic = \relative c' {
  c4 c f f2 f4 |
  e2 e4 f2. \bar "||"
  f4 e d c2 f4 |
  f2.( f) \bar "||"
  
  e4 ees ees d2 g4 |
  fis2 fis4 g2. \bar "||"
  g4 g g g2 g4 |
  fis2 fis4 d2 \bar "||"
  
  c4 |
  c2 c4 b2 f'4 |
  f( e d e2) \bar "||"
  c4 |
  c2 c4 c( des) c |
  b2 b4 c2 \bar "||"
  
  s4 |
  s1. |
  s2. s2 c4 |
  c2. c2( bes4) |
  c2.~ c2 c4 |
  c2.~ c2 c4 |
  
  d2. f~ |
  f) c2.~ |
  c2 c4 c( d) e |
  f2.~ f |
  
  e2.~ e |
  c~ c( |
  d2) d4 d2 d4 |
  c2.~ c |
  s1. |
  f2.~ f \bar "||"
  
  % verse 4
  c4 c f f2 f4 |
  e2 e4 f2 \bar "||"
  f4 |
  f4( e) d c2 f4 |
  f2. f \bar "||"
  
  e4 ees ees d2 g4 |
  fis2 fis4 g2. \bar "||"
  g4 g g g2 g4 |
  fis2 fis4 d2 \bar "||"
  
  c4 |
  c2 c4 b2 f'4 |
  f( e d) e2 \bar "||"
  c4 |
  c2. c |
  c c |
  
  c2 c4 c2 b4 |
  c2.~ c2 \bar "||"
  s4 |
  s1. |
  s2. s2 c4 |
  c2. c2( bes4) |
  
  c2.~ c2 c4~ |
  c2.~ c4 c c |
  d2( e4) f2. |
  f2.~ f( |
  
  e2 c4 d2) e4 |
  f2 c4 f2 f4 |
  e2 bes'4 a2 g4 |
  f2 s4 s2. |
  
  s2 f4 f2 f4 |
  f2. f |
  f e |
  c2 s4 s2. |
  
  s2 f4 f2 f4 |
  f2. f |
  f2( e4) d2( c4) |
  c2 f4 f2 f4 |
  
  f2.( g |
  c, g' |
  f) f2( g4) |
  f2.~ f( |
  e)~ e2 f4 |
  f1. \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  In -- fant of days, yet Lord of Life,
  Sweet Prince of Peace, All hail! __
  Oh! we are wea -- ry of the strife,
  The din with which earth’s fields are rife,
  And we would list the tale __
  That chimes its Christ -- mas news for us,
  
  \markup\italic “In \markup\italic ter -- \markup\italic ra __ \markup\italic Pax, __
  \markup\italic In \markup\italic ter -- \markup\italic ra \markup\italic Pax, \markup\italic Pax __ \markup\italic Pax __ \markup\italic ho -- \markup\italic mi -- \markup\italic ni -- \markup\italic bus, __
  \markup\italic Pax, __ \markup\italic Pax __ \markup\italic In \markup\italic ter -- \markup\italic ra \markup\italic Pax, __ \markup\italic Pax.” __
  
  O hear Thy Church, with one ac -- cord,
  Her long -- lost Peace im -- plor -- ing:
  Be it ac -- cord -- ing to Thy word:
  Thy Reign of Peace bring in, dear Lord;
  Heav’n’s Peace to earth re -- sto -- ring.
  And Peace E -- ter -- nal, Je -- su, grant, we pray.
  
  \markup\italic “In \markup\italic Cæ -- \markup\italic lo __ \markup\italic Pax, __
  \set associatedVoice = "sopranos"
  \markup\italic Et __ \markup\italic in \unset associatedVoice \markup\italic Ex -- \markup\italic cel -- \markup\italic sis,
  \set associatedVoice = "sopranos"
  \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
  
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic Glo -- \markup\italic ri -- \markup\italic a.
  
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel --
  \set associatedVoice = "tenors"
  \markup\italic sis, "" \markup\italic Glo -- _ \markup\italic ri -- _ \set associatedVoice = "altos" \markup\italic a,
  
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic Glo -- 
  \set associatedVoice = "sopranos"
  \markup\italic ri -- \markup\italic a.”
}
altoWordsII = \lyricmode {
%\markup\italic 
  \set stanza = #"2. "
  “Peace I leave with you,” was a -- gain
  Thy dy -- ing Gift to earth; __
  Sweet ech -- o of the lin -- g’ring strain
  Of Christ -- mas morn, the glad re -- frain
  Of An -- thems at Thy Birth; __
  When An -- gel choirs hymned forth to us
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  O ol -- ive Branch! O Dove of Peace!
  Brood -- ing o’er storm -- y
  \set ignoreMelismata = ##t
  wa -- ters!
  \unset ignoreMelismata
  When shall the flood of woe de -- crease?
  When shall the drear -- y con -- flict cease,
  And earth’s sad sons and
  \set ignoreMelismata = ##t
  daugh -- _ _ ters
  \unset ignoreMelismata
  With glad hearts hail __ Thy word to us,
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
  c4_\mf c c a2 c4 |
  c( bes) c a2( c4) \bar "||"
  d_\p c bes a2 a4 |
  b2.~ b \bar "||"
  
  c4 g g g( a) bes |
  c2 c4 bes2. \bar "||"
  bes4 c cis d( c) bes |
  bes( a) c bes2 \bar "||"
  
  g4_\f |
  a( g) f f( g) a |
  a( g f g2) \bar "||"
  c4 |
  c2 c4 c( bes) a |
  aes4.( g8) f4 e2 \bar "||"
  
  c'4\rest^\p |
  c1.\rest |
  c2.\rest c2\rest a4 |
  a2. f2( bes4) |
  a2.~ a2 a4 |
  c2( bes4 a2) g4 |
  
  a2( g4 f2. |
  a2) f4^\mf a2 d4 |
  c2 bes4 a2 g4 |
  a2.~ a( |
  
  bes2) g4 c2 bes4 |
  a2.~ a( |
  bes2) bes4 bes2 bes4 |
  a2.~ a |
  s1. |
  <c a>2.~_\pp q \bar "||"
  
  
  %verse 4
  c4 c c a2 c4 |
  c( bes) c a2 \bar "||"
  a4 |
  d( c) bes a2 a4 |
  b2. b \bar "||"
  
  c4 g g g( a) bes |
  c2 c4 bes2. \bar "||"
  bes4 c cis d( c) bes |
  bes( a) c bes2 \bar "||"
  
  g4_\f |
  a( g) f f( g) a |
  a( g f) g2 \bar "||"
  c4_\pp |
  c2. c |
  c c |

  bes2 a4 g2 f4 |
  e2.~ e2 \bar "||"
  c'4\rest^\f |
  c1.\rest |
  c2.\rest c2\rest a4 |
  a2. f2( bes4) |
  
  a2.~ a2 s4 |
  c2( bes4) a2 g4 |
  a2( g4) f2. |
  a2.( f |
  
  g2.) c, |
  a'~ a |
  bes2 d4 c2 bes4 |
  a2. a |
  
  a2 b4 b2 g4 |
  a2. c |
  bes g |
  a2 c4\rest c2.\rest |
  
  c2\rest a4 a2 a4 |
  a2 f4 g2 a4 |
  bes2 c4 d2 e4 |
  f2 s4 s2. |
  
  s2 a,4 c2 bes4 |
  a2.( g2 a4) |
  bes2( c4 d2.) |
  c2.~ c |
  g2.( bes) |
  a1. \bar "|."
}
tenorWords = \lyricmode {
  \repeat unfold 44 {\skip 1}
  \repeat unfold 7 {\skip 1}
  \markup\italic Pax, __ \markup\italic In \markup\italic ter -- \markup\italic ra \markup\italic Pax \markup\italic ho -- \markup\italic mi -- \markup\italic ni -- \markup\italic bus, __
  \markup\italic ho -- \markup\italic mi -- \markup\italic ni -- \markup\italic bus __ \markup\italic In \markup\italic ter -- \markup\italic ra \markup\italic Pax, __ \markup\italic Pax.” __ ""
  
  \repeat unfold 51 {\skip 1}
  \markup\italic Et __ \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic Glo -- \markup\italic ri -- \markup\italic a, __
  \markup\italic Glo -- \markup\italic ri -- \markup\italic a, \markup\italic In \markup\italic Cæ -- \markup\italic lo \markup\italic Pax,
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic Glo -- \markup\italic ri -- \markup\italic a.
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, __ \markup\italic Glo -- \markup\italic ri -- \markup\italic a.”
}

bassMusic = \relative c {
  a'4 a f c( f) a |
  c2 c,4 f2. \bar "||"
  f4 f f f2 d4 |
  g2.~ g \bar "||"
  
  c,4 c c d2 d4 |
  d2 d4 g2( f4) \bar "||"
  ees4 ees ees d2 d4 |
  d2 d4 g2 \bar "||"
  
  e4 |
  f2 f4 d2 d4 |
  c2.~ c2 \bar "||"
  c4 |
  c2 c4 c2 c4 |
  c2 c4 c2 \bar "||"
  
  c4 |
  f2. d |
  f~ f2 f4 |
  f2. bes, |
  f~ f2 f'4 |
  a2( g4 f2) e4 |
  
  d2.~ d |
  c~ c~ |
  c~ c~ |
  c~ c~ |
  c2 c4 c( d) e |
  f2 f4 f2 f4 |
  bes,2.~ bes |
  f'~ f |
  d1.\rest |
  <c f,>2.~ q\fermata \bar "||"
  
  %verse 4
  a'4 a f c( f) a |
  c2 c,4 f2 \bar "||"
  f4 |
  f2 f4 f2 d4 |
  g2. g \bar "||"
  
  c,4 c c d2 d4 |
  d2 d4 g2( f4) \bar "||"
  ees4 ees ees d2 d4 |
  d2 d4 g2 \bar "||"
  
  e4 |
  f2 f4 d2 d4 |
  c2. c2 \bar "||"
  c4 |
  c2. c2. |
  c c |
  
  c2 c4 c2 c4 |
  c2.~ c2 \bar "||"
  c4 |
  f2. d |
  f~ f2 f4 |
  f2. bes, |
  f~ f2 d'4\rest |
  a'2( g4) f2 e4 |
  d2.~ d |
  c2 f4 a2 d4 |
  
  c2 bes4 a2 g4 |
  f2. c2 c4 |
  c2.~ c |
  d2 b4\rest b2.\rest |
  
  b2\rest g4 g2 |
  b4 |
  c2.~ c |
  c~ c2 c4 |
  f2. f |
  
  f2 d4 c2 b4 |
  c2. c |
  c c |
  f2 d4\rest d2.\rest |
  
  d2\rest d4 e2 e4 |
  f2.( e) |
  d( bes) |
  c~ c~ |
  c c |
  <f f,>1. \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 44 {\skip 1}
  \markup\italic In \markup\italic ter -- \markup\italic ra \markup\italic Pax
  \repeat unfold 7 {\skip 1}
  \markup\italic Pax, __ \markup\italic Pax, __ \markup\italic ho -- \markup\italic mi -- \markup\italic ni -- \markup\italic bus, \markup\italic In \markup\italic ter -- \markup\italic ra \markup\italic Pax. __ ""
  
  \repeat unfold 49 {\skip 1}
  \markup\italic “In \markup\italic Cæ -- \markup\italic lo \markup\italic Pax,
  \repeat unfold 4 {\skip 1}
  \markup\italic Et __ \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis,
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
  \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis,
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, __
  \markup\italic In \markup\italic cæ -- \markup\italic lo, \markup\italic Pax,
  \markup\italic Et \markup\italic in \markup\italic Ex -- \markup\italic cel -- \markup\italic sis, \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
  \repeat unfold 5 {\skip 1}
  \markup\italic Glo -- \markup\italic ri -- \markup\italic a.”
}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
  }
}
