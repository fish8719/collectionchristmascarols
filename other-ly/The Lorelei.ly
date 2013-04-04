\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Lorelei"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Die Lorelei)"}}
  composer = \markup\oldStyleNum"Friedrich Silcher (1789–1860)"
  poet = \markup\oldStyleNum"Heinrich Heine (1797–1856)"
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
  \key d \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  a'8 |
  a8.[ b16] a8 d[ cis] b |
  a4.\> g4\! g8 |
  fis8.[ fis16] fis8 e[ d] e |
  fis4.~ fis8 b\rest a |
  
  a8.[ b16] a8 d[ cis] b |
  a4.\> g4\! g8 |
  fis8.[ fis16] fis8 a[ g] e |
  d4.~ d8 bes'\rest fis |
  
  e8.[ fis16] e8 a[ e] a |
  cis4.\< b4 b8 |
  a8[\> a] a gis[ a]\! b |
  a4.~\< a4\> a8\! |
  
  a8. b16 a8 d[\< cis] b^\markup\italic"poco rit." |
  a4( fis'8) e4\fermata e16[ e] |
  d8.[ d16] d8 cis[ b] cis |
  d4.~ d8 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	I know not \set ignoreMelismata = ##t what is the \unset ignoreMelismata mean -- ing that wear -- y, sad am I,
  Of an -- cient times I’m dream -- ing a leg -- end long gone by;
  \set ignoreMelismata = ##t
  The day _ is fad -- ing to twi -- light, and soft -- _ ly flows _ the Rhine, _
  \unset ignoreMelismata
  The moun -- tains a -- far are gleam -- ing, in sun -- set’s gold -- en shine.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  A -- bove on the rocks _ is lean -- ing a maid -- _ en strange -- _ ly fair, _
  Her gold -- _ en jew -- els are gleam -- ing, she combs _ her long gold -- en hair; _
  She combs it with fair -- y comb gold -- en, a song _ the while _ sings she, _
  Of mean -- ing un -- earth -- ly and old -- _ en, a _ pow -- er -- ful mel -- _ o -- dy. __ _
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  A boat -- man be -- low _ is heark -- ’ning, it fills him with grief _ and love; _
  He heeds not the rocks _ so dark -- ’ning, he sees but the form _ a -- bove. _
  Ah me! that the waves will have swal -- lowed both boat -- man and boat _ ere long, _
  And this by the charm _ un -- hal -- _ lowed of the Lo -- _ re -- lei with her song. _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 8
  fis8 |
  fis8.[ g16] fis8 b[ a] g |
  fis4. e4 e8 |
  d8.[ d16] d8 cis[ b] cis |
  d4.~ d8 s fis |
  
  fis8.[ g16] fis8 b[ a] g |
  fis4. e4 e8 |
  d8.[ d16] d8 fis[ e] cis |
  d4.~ d8 s d |
  
  cis8.[ d16] cis8 e[ cis] e |
  a4. fis4 fis8 |
  e[ e] e e[ e] d |
  cis[ e fis g fis] e |
  
  fis8. g16 fis8 b[ a] g |
  fis4( a8) g4 g16[ g] |
  fis8.[ fis16] fis8 e[ d] e |
  fis4.~ fis8 s \bar"|."
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
  \partial 8
  d8 |
  d8.[ d16] d8 d[ d] d |
  d4. b4 b8 |
  a8.[ a16] a8 a4 a8 |
  a4.~ a8 s d |
  
  d8.[ d16] d8 d[ d] d |
  d4. b4 b8 |
  a8.[ a16] a8 a[ a] g |
  fis4.~ fis8 s a |
  
  a8.[ a16] a8 cis[ a] cis |
  e4. d4 d8 |
  cis[ cis] cis b[ a] gis |
  a[ cis d e d] cis |
  
  d8. d16 d8 d[ d] d |
  d4. b4 b16[ b] |
  a8.[ a16] a8 a4 a8 |
  a4.~ a8 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 8
  d,8 |
  d8.[ d16] d8 d[ d] d |
  d4. g4 g8 |
  a8.[ a16] a8 a,4 a8 |
  d4.~ d8 d8\rest  d |
  
  d8.[ d16] d8 d[ d] d |
  d4. g4 g8 |
  a8.[ a16] a8 a,[ a] a |
  d4.~ d8 d\rest d |
  
  a8.[ a16] a8 a[ a] a |
  a'4. d,4 d8 |
  e[ e] e e[ e] e |
  a4.~ a4 a8 |
  
  d,8. d16 d8 d[ d] d |
  d4.-> g4\fermata g16[ g] |
  a8.[ a16] a8 a,4 a8 |
  d4.~ d8 d\rest \bar"|."
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
      \new Voice = "sopranos" \transpose d des { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" \transpose d des { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" \transpose d des { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" \transpose d des { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 60
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
