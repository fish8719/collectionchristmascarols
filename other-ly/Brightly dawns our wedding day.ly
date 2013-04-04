\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Brightly dawns our wedding day"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"Arthur Sullivan (1842–1900)"
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \repeat volta 2
  {
    b'1\rest |
    b\rest |
    b\rest |
    b\rest |
    b2\rest f4. a8 |
    
    c4 c c c |
    c2 c4 d8[ e] |
    f4 d c bes |
    a8[ bes] c4 c d8[ e] |
    f4 c c bes |
    
    a8[ bes] c4 a c |
    b c d e |
    f2 f4 e8[ d] |
    e4 c d b |
    c2 b\rest |
    
    %page2/125 Soprano
    \break\oneVoice
    \clef bass
    d,,1\rest |
    d2\rest c'4 c |
    a bes8[ c] d4 d |
    bes4 g \clef treble \stemDown bes' bes |
    g a8[ bes] c4 c |\break
    
    a f \stemNeutral \voiceOne a c 
    d c d ees |
    c2 r4\f f~ |
    f ees8[ d] ees4 ees |
    ees d8[ c] d4 d~ |
    
    d c8[ bes] c4 c |
    c bes8[ a] <<bes4 {s8. s16\p}>> d4(\< |
    ees1)\> |
    d2\! b4\rest d4(\< |
    ees1)\> |
    d2\! d4 bes |
    
    %page3/126 Soprano
    g4 g a bes8[ c] |
    g2 c4 a |
    g g a bes8[ c] |
    g2 f4.\f a8 |
    c4 c c c |
    c2 c4\f d8[ e] |
    f4 c c bes |
    a8[( bes] c4) c d8[ e] |
    f4 c c bes |
    a8([ bes] c4) d2 |
    
    %page4/127 soprano
    g,2 c |
    f, bes~ |
    bes8 a\ff g f e4 c'~ |
    c8 bes a g f4 d'~ |
    
    d8 c bes a g4 g'~ |
    g8 f e d c bes a g |
    a4 b\rest r c\dim |
    d c bes c8[ d] |
    c4 b\rest b\rest a |
    
    bes a g a8[ g] |
    c4 a\p g2 |
    a4 a c2 |
    a b\rest |
    b\rest a\pp |
    g1~ |
    g
  }
  \alternative {
    {
      f2. b4\rest
    }
    {
      f1\fermata \bar"|."
    }
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Bright -- ly dawns our wed -- ding day;
  Joy -- ous hour, we give thee greet -- ing!
  Whi -- ther, whi -- ther art thou fleet -- ing?
  Fick -- le mo -- ment, pri -- thee stay!
  Fick -- le mo -- ment, pri -- thee stay!
  
  %page2/125
  What though mor -- tal joys be hol -- low?
  Plea -- sures come, if sor -- rows fol -- low:
  Though the toc -- sin sound ere long,
  
  Though the toc -- sin sound ere long,
  Though the toc -- sin sound ere long,
  Ding dong!
  Ding dong!
  
  Yet un --
  
  %page3/126
  til the shad -- ows fall
  O -- ver one and o -- ver all,
  \dropLyricsXII Sing a mer -- ry mad -- ri -- gal,
  Sing a mer -- ry mad -- ri -- gal,
  Sing a \raiseLyrics mer -- ry mad -- ri -- gal,
  Fa
  
  %page4/127
  la.
  Fa la.
  Fa la la la la,
  Fa la la la la,
  Fa la la la la,
  Fa la la la la la la la la,
  la la la la,
  Fa la,
  Fa la la la,
  Fa la,
  Fa la la,
  Fa la la,
  Fa la __ la.
  la.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Let us dry the rea -- dy tear,
  Though the hours are sure -- ly creep -- ing,
  Lit -- tle need for woe -- ful weep -- ing,
  Till the sad sun -- down is near,
  Till the sad sun -- down is near.
  
  %page2/125
  All must sip the cup of sor -- row
  I to day, and thou to -- mor -- row:
  This the close of ev -- ’ry song,
  
  This the close of ev -- ’ry song,
  This the close of ev -- ’ry song,
  Ding dong!
  Ding dong!
  
  What, though
  
  %page3/126
  sol -- emn shad -- ows fall,
  Soon -- er, lat -- er, o -- ver all.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
    \repeat volta 2 {
    s1*5 |
    
    s1 |
    s2 e4 f8[ g] |
    f4 a g e |
    f f c c' |
    c a g e |
    
    f f f a |
    a g f e |
    a2 g4 g |
    g a a g |
    g2 s |
    
    %page2/125 alto
    s1*5 |
    s2 f4 a |
    bes a bes bes |
    a r bes2 |
    bes, bes' |
    bes, bes' |
    
    bes, bes' |
    bes,2. f'4( |
    g1) |
    f2 s4 f( |
    g1) |
    f2 f4 g |
    
    %page3/126 alto
    e4 e f g8[ a] |
    g2 f4 f |
    e e f g8[ a] |
    e2 s |
    s1 |
    
    s2 e4 f8[ g] |
    f4 a g e |
    f2 c4 c' |
    c a g e |
    f2 f |
    
    %page4/127 alto
    g8 f e d c4 d8 e |
    f e d c bes4 c8 d |
    e4 g c,2~ |
    c4 e d2~ |
    
    d4 f e8 f g a |
    bes2 e,4 e |
    f s f2~ |
    f f |
    f4 s s f |
    
    f f e e |
    f f e2 |
    f4 f g2 |
    f s |
    s f |
    f1( |
    e) |
  }
  \alternative {
    {
      f2. s4 |
    }
    {
      f1 \bar"|."
    }
  }
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
    \repeat volta 2 {
    s1*5 |
    
    s1 |
    s2 c4 c |
    c c c c |
    c c e f8[ g] |
    c,4 c c c |
    
    c c c f |
    f e d c |
    c2 d4 b |
    c4 e d d |
    e2 s |
    
    %page2/125 tenor
    s1*5 |
    
    s2 c4 f |
    f f f g |
    f( c) d2 |
    r4 g2 f8[ ees] |
    f4 f f ees8[ d] |
    
    ees4 ees2 d8[ c] |
    d2. r4 |
    bes1 |
    bes2 s4 r |
    bes1 |
    bes2 bes4 d |
    
    %page3/126 tenor
    c4 c c c |
    c2 c4 c |
    c c c bes8[ a] |
    c2 s |
    s1 |
    
    s2 c4 c |
    c c c c |
    c2 e4 f8[ g] |
    c,4 c c c |
    c2 bes~ |
    
    %page4/127 tenor
    bes4 bes a2~ |
    a4 a g2~ |
    g4 bes4~ bes8 a g f |
    e4 c'~ c8 bes a g |
    
    f4 d'~ d8 c bes a |
    g4 e'8 f g4 c, |
    c r s a |
    bes a g a8[ bes] |
    a4 s r c |
    
    d c bes bes |
    a c c2 |
    c4 c c2 |
    c s |
    s c |
    d1( |
    bes) |
  }
  \alternative {
    {
      a2. s4 |
    }
    {
      a1 \bar"|."
    }
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
    \repeat volta 2 {
    d,1\rest |
    d\rest |
    d\rest |
    d\rest |
    d\rest |
    
    d\rest |
    d2\rest c'4 bes |
    a f e c |
    f a c bes |
    a f e c |
    
    f a f d |
    g a b c |
    a2 b4 g |
    c a f g |
    c,2 d\rest |
    
    %page2/125 bass
    d1\rest |
    d1\rest |
    d1\rest |
    d1\rest |
    d1\rest |
    
    d2\rest f4 f |
    bes f bes ees, |
    f r bes2 |
    bes, bes' |
    bes, bes' |
    
    bes, bes' |
    bes,2. bes'4( |
    ees,1) |
    bes2 d4\rest bes'4( |
    ees,1) |
    bes2 bes'4 g |
    
    %page3/126 bass
    c4 bes a f |
    c2 a'4 f |
    c' bes a g8[ f] |
    c2 d2\rest |
    d1\rest |
    
    d2\rest c'4 bes |
    a f e c |
    f( a) c bes |
    a f e c |
    f8[( g] a4) bes8 a g f |
    
    %page4/127 bass
    e4 f8 g a g f e |
    d4 e8 f g f e d |
    c1 |
    c |
    
    c2. g'8 f |
    e d c4 c'4. bes8 |
    a g f4 d\rest f |
    f f f f |
    f d\rest f2~ |
    
    f f |
    f4 f f2 |
    f4 f f2 |
    f d\rest |
    d\rest a |
    bes1( |
    c) |
  }
  \alternative {
    {
      f2. d4\rest |
    }
    {
      f1\fermata \bar"|."
    }
  }
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
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.1
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

\score {
  \unfoldRepeats
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "flute"
  }
}
