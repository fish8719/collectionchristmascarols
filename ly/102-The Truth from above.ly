\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Truth From Above"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #10.5 "(Herefordshire Carol)"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Traditional"  
  tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}
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
  first-page-number = #102
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
  \key c \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \repeat volta 3 {
    \partial 4 e4 |
    a b \times 2/3 { c( b) a} |
    \partial 4*6 g a e2. b'4 |
    c c \times 2/3 {b( a) g} |
    
    \partial 4*6 a b c2. \bar""\break c8[ d] |
    e4 e d c8[ b] |
    \partial 4*6 a4 a e2. \slurDotted e8( g) |
    
    a4 b c8[ d] e[ d] |
    c[ a] b4 a2
  }
  \break
  
  
  
  \repeat volta 2 {
    \partial 4 e4 |
    a b \times 2/3 { c( b) a} |
    \partial 4*6 g a e2. b'4 |
    c c \times 2/3 {b( a) g} |
    
    \partial 4*6 a b c2. \bar""\break c8[ d] |
    e4 e d c8[ b] |
    \partial 4*6 a4 a e2. e8[ g] |
    
    a4 b c8[ d] e[ d] |
    c[ a] b4 a2
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 |
  a a \times 2/3 {a( g) f} |
  e d b2. e4 |
  e a \times 2/3 {g( f) e} |
  
  f4 f f( e2) a4 |
  g g8[ a] d,4 a'8[ g] |
  e4 fis b,2. \slurDotted e8( e) |
  
  e4 e a16[ g fis8] e[ fis] |
  e4 e e2
  
  
  
  e4 |
  a a \times 2/3 {a( g) f} |
  e d b2. e4 |
  e a \times 2/3 {g( f) e} |
  
  f4 f f( e2) a4 |
  g g8[ a] d,4 a'8[ g] |
  e4 fis b,2. e8[ e] |
  
  e4 e a16[ g fis8] e[ fis] |
  e4 e e2
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  This is the truth sent from a -- bove,
  The truth of God, the God of love.
  There -- fore don’t turn me from your door,
  But __ heark -- en all both rich and poor.
  
  \set stanza = #"4."
  And at that sea -- son of the year
  Our blest re -- deem -- er did ap -- pear;
  He here did live, and here did preach,
  and ma -- ny thou -- sands He did teach.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  The first things which I do re -- late
  \skip1
  Is that God did man cre -- ate;
  The next thing which to you I’ll tell
  Wo -- man was made with man to dwell.
  
  \set stanza = #"5."
  Thus He in love to us be -- haved,
  To show us how we must be saved;
  And if you want to know the way,
  Be pleased to hear what He did say.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  Thus we were heirs to end -- less woes,
  Till God the Lord did in -- ter -- pose;
  And so a prom -- ise soon did run
  \set ignoreMelismata = ##t
  That he
  \unset ignoreMelismata
  would re -- deem us by His Son.

}
altoWordsIV = \lyricmode {
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
  e4 |
  c d \times 2/3 {e2 c4} |
  c a a( g2) b4 |
  a e' \times 2/3 {e( c) c} |
  
  c d g,2. a4 |
  b c b e8[ d] |
  c4 a g2( fis!4) \slurDotted g8( b) |
  
  c4 b a8[ b] c[ a] |
  g4 b c2
  
  
  e4 |
  c d \times 2/3 {e2 c4} |
  c a a( g2) b4 |
  a e' \times 2/3 {e( c) c} |
  
  c d g,2. a4 |
  b c b e8[ d] |
  c4 a g2( fis!4) g8[ b] |
  
  c4 b a8[ b] c[ a] |
  g4 b c2
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 |
  a2 \times 2/3 {a4( e) f} |
  c4 f e2. g4 |
  a a \times 2/3 {e( f) c} |
  
  f8[ e] d4 c2. f4 |
  e e8[ fis] g[ g,] a[ b] |
  c4 d e2. \slurDotted e8( e) |
  
  a4 g f16[ e d8] c[ d] |
  e4 g a2
  
  
  
  e4 |
  a2 \times 2/3 {a4( e) f} |
  c4 f e2. g4 |
  a a \times 2/3 {e( f) c} |
  
  f8[ e] d4 c2. f4 |
  e e8[ fis] g[ g,] a[ b] |
  c4 d e2. e8[ e] |
  
  a4 g f16[ e d8] c[ d] |
  e4 g a2
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
