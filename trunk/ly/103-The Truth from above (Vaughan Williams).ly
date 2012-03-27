\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Truth From Above"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #10.5 "(Herefordshire Carol)"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Arranged by Ralph Vaughan Williams (1872-1958)"
  tagline = ""
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
  first-page-number = #103
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
#(define ((compound-time one two num numtwo) grob)
   (grob-interpret-markup grob
                          (markup #:override '(baseline-skip . 0) #:number
                                  (#:line ((#:column (one num))
                                           (#:column (two numtwo)))))))
global = {
  \key c \major
  \autoBeamOff
  \override Staff.TimeSignature #'stencil = #(compound-time "5" "3" "4" "2")
  \time 11/4
}

sopMusic = \relative c' {
  \repeat volta 3 {
    \partial 2 e2 |
    \partial 4*5 a4 b c( b) a |
    \partial 2*3 g a e2. b'4 |
    
    \partial 4*5 c c b( a) g |
    \partial 2*3 a b c2. \bar""\break c8[ d] |
    \partial 2*3 e4 e d2 c4( b) |
    
    \partial 4*5 \slurDotted a a e2 e8( g) |
    \slurSolid
    \partial 2*3 a4 b c( d) e( d) |
    \partial 1 c8[ a] b4 a2
  }
  \break
  
  
  \repeat volta 2 {
    \partial 2 e2 |
    \partial 4*5 a4 b c( b) a |
    \partial 2*3 g a e2. b'4 |
    
    \partial 4*5 c c b( a) g |
    \partial 2*3 a b c2. \bar""\break c8[ d] |
    \partial 2*3 e4 e d2 c4( b) |
    
    \partial 4*5 a a e2 e8[ g] |
    \partial 2*3 a4 b c( d) e( d) |
    \partial 1 c8[ a] b4 a2
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e2 |
  a4 a a( g) f |
  e d b2. e4 |
  
  e a g( f) e |
  f f f( e2) a4 |
  g g8[ a] d,4( g) c,( d) |
  
  \slurDotted e fis b,2 e8( g) |
  \slurSolid a4 e a8([ g] fis4) e( fis) |
  g8[ a] g[ fis] e2
  
  
  
  e2 |
  a4 a a( g) f |
  e d b2. e4 |
  
  e a g( f) e |
  f f f( e2) a4 |
  g g8[ a] d,4( g) c,( d) |
  
  e fis b,2 e8[ g] |
  a4 e a8([ g] fis4) e( fis) |
  g8[ a] g[ fis] e2
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
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  e2 |
  c4 d e2 c4 |
  c a a( g2) b4 |
  
  a e' e( c) c |
  c d g,2. a4 |
  b c b2 e4( d) |
  
  c a g( fis) \slurDotted g8( b) |
  \slurSolid c4 b a4.( b8) c4( a) |
  e'4 d cis2
  
  
  
  e2 |
  c4 d e2 c4 |
  c a a( g2) b4 |
  
  a e' e( c) c |
  c d g,2. a4 |
  b c b2 e4( d) |
  
  c a g( fis) g8[ b] |
  c4 b a4.( b8) c4( a) |
  e'4 d cis2
}
tenorWords = \lyricmode {
  
}

bassMusic = \relative c {
  e2 |
  a4 a a( e) f |
  c f e2. g4 |
  
  a4 a e( f) c |
  f8[ e] d4 c2. f4 |
  e e8[ fis] g4( g,) a( b) |
  
  c4 d e2 \slurDotted e8( e) |
  a4 g \slurSolid f8([ e] d4) c( d) |
  e8[ fis] g4 a2
  
  
  
  
  
  e2 |
  a4 a a( e) f |
  c f e2. g4 |
  
  a4 a e( f) c |
  f8[ e] d4 c2. f4 |
  e e8[ fis] g4( g,) a( b) |
  
  c4 d e2 e8[ e] |
  a4 g f8([ e] d4) c( d) |
  e8[ fis] g4 a2
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
  }
}
