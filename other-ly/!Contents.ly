\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Contents"}}
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  ragged-last-bottom = ##t
  ragged-bottom = ##t
  two-sided = ##t
  ragged-right = ##f
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #1
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\markup \vspace #1
\markup {\page-link #142 {\fill-with-pattern #0.2 #LEFT . "Abide with me" \smallCapsOldStyle 139}}
\markup {\page-link #108 {\fill-with-pattern #0.2 #LEFT . "America" \smallCapsOldStyle 105}}
\markup {\page-link #57 {\fill-with-pattern #0.2 #LEFT . "America the Beautiful" \smallCapsOldStyle 54}}
\markup {\page-link #46 {\fill-with-pattern #0.2 #LEFT . "Am I Not Fondly Thine Own" \smallCapsOldStyle 43}}
\markup {\page-link #48 {\fill-with-pattern #0.2 #LEFT . "Annie Laurie" \smallCapsOldStyle 45}}
\markup {\page-link #143 {\fill-with-pattern #0.2 #LEFT . "Be Still, My Soul" \smallCapsOldStyle 140}}
\markup {\page-link #65 {\fill-with-pattern #0.2 #LEFT . "Boating Song" \smallCapsOldStyle 62}}
\markup {\page-link #37 {\fill-with-pattern #0.2 #LEFT . "Bonnie Doon" \smallCapsOldStyle 34}}
\markup {\page-link #87 {\fill-with-pattern #0.2 #LEFT . "Bride Bells" \smallCapsOldStyle 84}}
\markup {\page-link #123 {\fill-with-pattern #0.2 #LEFT . "Brightly dawns our wedding day" \smallCapsOldStyle 120}}
\markup {\page-link #52 {\fill-with-pattern #0.2 #LEFT . "By the Sad Sea Waves" \smallCapsOldStyle 49}}
\markup {\page-link #126 {\fill-with-pattern #0.2 #LEFT . "Catch Round the Table (Now we are met)" \smallCapsOldStyle 123}}
\markup {\page-link #136 {\fill-with-pattern #0.2 #LEFT . "Come, Thou Fount of Every Blessing" \smallCapsOldStyle 133}}
\markup {\page-link #21 {\fill-with-pattern #0.2 #LEFT . "Come again, sweet love" \smallCapsOldStyle 18}}
\markup {\page-link #72 {\fill-with-pattern #0.2 #LEFT . "Come Follow (Round)" \smallCapsOldStyle 69}}
\markup {\page-link #72 {\fill-with-pattern #0.2 #LEFT . "Come Follow Me Merrily (Round)" \smallCapsOldStyle 69}}
\markup {\page-link #110 {\fill-with-pattern #0.2 #LEFT . "Come Let Us All A-Maying Go (Round)" \smallCapsOldStyle 107}}
\markup {\page-link #39 {\fill-with-pattern #0.2 #LEFT . "Could I a maiden find" \smallCapsOldStyle 36}}
\markup {\page-link #76 {\fill-with-pattern #0.2 #LEFT . "Darby and Joan" \smallCapsOldStyle 73}}
\markup {\page-link #45 {\fill-with-pattern #0.2 #LEFT . "De Brevitate Vitæ (Gaudeamus Igitur)" \smallCapsOldStyle 42}}
\markup {\page-link #59 {\fill-with-pattern #0.2 #LEFT . "Dixie" \smallCapsOldStyle 56}}
\markup {\page-link #51 {\fill-with-pattern #0.2 #LEFT . "Dreaming of Home and Mother" \smallCapsOldStyle 48}}
\markup {\page-link #75 {\fill-with-pattern #0.2 #LEFT . "Dublin Bay" \smallCapsOldStyle 72}}
\markup {\page-link #88 {\fill-with-pattern #0.2 #LEFT . "Ego sum pauper (Round)" \smallCapsOldStyle 85}}
\markup {\page-link #41 {\fill-with-pattern #0.2 #LEFT . "Ein Prosit" \smallCapsOldStyle 38}}
\markup {\page-link #93 {\fill-with-pattern #0.2 #LEFT . "Fairy Belle" \smallCapsOldStyle 90}}
\markup {\page-link #36 {\fill-with-pattern #0.2 #LEFT . "Flow Gently, Sweet Afton" \smallCapsOldStyle 33}}
\markup {\page-link #106 {\fill-with-pattern #0.2 #LEFT . "For he’s a jolly good fellow (We won’t go home until morning)" \smallCapsOldStyle 103}}
\markup {\page-link #88 {\fill-with-pattern #0.2 #LEFT . "Gaudeamus Hodie (Round)" \smallCapsOldStyle 85}}
\markup {\page-link #45 {\fill-with-pattern #0.2 #LEFT . "Gaudeamus Igitur (De Brevitate Vitæ)" \smallCapsOldStyle 42}}
\markup {\page-link #135 {\fill-with-pattern #0.2 #LEFT . "Glorious Things of Thee Are Spoken" \smallCapsOldStyle 132}}
\markup {\page-link #139 {\fill-with-pattern #0.2 #LEFT . "Glory be to Jesus" \smallCapsOldStyle 136}}
\markup {\page-link #102 {\fill-with-pattern #0.2 #LEFT . "God be with you till we meet again" \smallCapsOldStyle 99}}
\markup {\page-link #141 {\fill-with-pattern #0.2 #LEFT . "God so loved the world" \smallCapsOldStyle 138}}
\markup {\page-link #100 {\fill-with-pattern #0.2 #LEFT . "Good Night Ladies" \smallCapsOldStyle 97}}
\markup {\page-link #4 {\fill-with-pattern #0.2 #LEFT . "Hail! Smiling Morn" \smallCapsOldStyle 1}}
\markup {\page-link #96 {\fill-with-pattern #0.2 #LEFT . "Happy Hours at Home" \smallCapsOldStyle 93}}
\markup {\page-link #95 {\fill-with-pattern #0.2 #LEFT . "Hard Times" \smallCapsOldStyle 92}}
\markup {\page-link #37 {\fill-with-pattern #0.2 #LEFT . "Hark! the vesper hymn is stealing" \smallCapsOldStyle 34}}
\markup {\page-link #114 {\fill-with-pattern #0.2 #LEFT . "He that Will an Alehouse Keep (Round)" \smallCapsOldStyle 111}}
\markup {\page-link #78 {\fill-with-pattern #0.2 #LEFT . "Home Sweet Home" \smallCapsOldStyle 75}}
\markup {\page-link #54 {\fill-with-pattern #0.2 #LEFT . "How can I leave thee" \smallCapsOldStyle 51}}
\markup {\page-link #6 {\fill-with-pattern #0.2 #LEFT . "How Lovely Is the Evening (Round)" \smallCapsOldStyle 3}}
\markup {\page-link #33 {\fill-with-pattern #0.2 #LEFT . "I dreamt I dwelt in marble halls" \smallCapsOldStyle 30}}
\markup {\page-link #46 {\fill-with-pattern #0.2 #LEFT . "Integer Vitae" \smallCapsOldStyle 43}}
\markup {\page-link #38 {\fill-with-pattern #0.2 #LEFT . "In the Spring" \smallCapsOldStyle 35}}
\markup {\page-link #15 {\fill-with-pattern #0.2 #LEFT . "It was a lover and his lass" \smallCapsOldStyle 12}}
\markup {\page-link #89 {\fill-with-pattern #0.2 #LEFT . "Jamie’s on the Stormy Sea" \smallCapsOldStyle 86}}
\markup {\page-link #71 {\fill-with-pattern #0.2 #LEFT . "Jenny the Flower of Kildare" \smallCapsOldStyle 68}}
\markup {\page-link #137 {\fill-with-pattern #0.2 #LEFT . "Jesus, Lover of my soul" \smallCapsOldStyle 134}}
\markup {\page-link #132 {\fill-with-pattern #0.2 #LEFT . "Jesus! the very thought of Thee" \smallCapsOldStyle 129}}
\markup {\page-link #35 {\fill-with-pattern #0.2 #LEFT . "John Anderson, my jo" \smallCapsOldStyle 32}}
\markup {\page-link #113 {\fill-with-pattern #0.2 #LEFT . "Johnny Sands" \smallCapsOldStyle 110}}
\markup {\page-link #31 {\fill-with-pattern #0.2 #LEFT . "Killarney" \smallCapsOldStyle 28}}
\markup {\page-link #117 {\fill-with-pattern #0.2 #LEFT . "La ci darem la mano" \smallCapsOldStyle 114}}
\markup {\page-link #139 {\fill-with-pattern #0.2 #LEFT . "Lead Kindly Light" \smallCapsOldStyle 136}}
\markup {\page-link #10 {\fill-with-pattern #0.2 #LEFT . "Let Us Sing (The Waits)" \smallCapsOldStyle 7}}
\markup {\page-link #91 {\fill-with-pattern #0.2 #LEFT . "Listen to the Mocking Bird" \smallCapsOldStyle 88}}
\markup {\page-link #9 {\fill-with-pattern #0.2 #LEFT . "Live we singing" \smallCapsOldStyle 6}}
\markup {\page-link #49 {\fill-with-pattern #0.2 #LEFT . "Loch Lomond" \smallCapsOldStyle 46}}
\markup {\page-link #66 {\fill-with-pattern #0.2 #LEFT . "Long, Long Ago" \smallCapsOldStyle 63}}
\markup {\page-link #68 {\fill-with-pattern #0.2 #LEFT . "Love’s Chidings" \smallCapsOldStyle 65}}
\markup {\page-link #32 {\fill-with-pattern #0.2 #LEFT . "Love’s Young Dream" \smallCapsOldStyle 29}}
\markup {\page-link #109 {\fill-with-pattern #0.2 #LEFT . "Maid of Athens" \smallCapsOldStyle 106}}
\markup {\page-link #126 {\fill-with-pattern #0.2 #LEFT . "Merrily Greet the Morn (Round)" \smallCapsOldStyle 123}}
\markup {\page-link #99 {\fill-with-pattern #0.2 #LEFT . "Merrily Sing" \smallCapsOldStyle 96}}
\markup {\page-link #130 {\fill-with-pattern #0.2 #LEFT . "Mister Speaker, though ’tis late (Round)" \smallCapsOldStyle 127}}
\markup {\page-link #13 {\fill-with-pattern #0.2 #LEFT . "My bonny lass she smileth" \smallCapsOldStyle 10}}
\markup {\page-link #94 {\fill-with-pattern #0.2 #LEFT . "My Old Kentucky Home" \smallCapsOldStyle 91}}
\markup {\page-link #79 {\fill-with-pattern #0.2 #LEFT . "Nancy Lee" \smallCapsOldStyle 76}}
\markup {\page-link #47 {\fill-with-pattern #0.2 #LEFT . "Night Song" \smallCapsOldStyle 44}}
\markup {\page-link #11 {\fill-with-pattern #0.2 #LEFT . "Now is the month of maying" \smallCapsOldStyle 8}}
\markup {\page-link #126 {\fill-with-pattern #0.2 #LEFT . "Now we are met (Catch Round the Table)" \smallCapsOldStyle 123}}
\markup {\page-link #41 {\fill-with-pattern #0.2 #LEFT . "O Calm of Night" \smallCapsOldStyle 38}}
\markup {\page-link #134 {\fill-with-pattern #0.2 #LEFT . "Ode to Joy" \smallCapsOldStyle 131}}
\markup {\page-link #55 {\fill-with-pattern #0.2 #LEFT . "O Fair Dove, O Fond Dove" \smallCapsOldStyle 52}}
\markup {\page-link #29 {\fill-with-pattern #0.2 #LEFT . "Oft in the stilly night" \smallCapsOldStyle 26}}
\markup {\page-link #140 {\fill-with-pattern #0.2 #LEFT . "Oh, happy is the man that hears" \smallCapsOldStyle 137}}
\markup {\page-link #100 {\fill-with-pattern #0.2 #LEFT . "Oh My Love (Round)" \smallCapsOldStyle 97}}
\markup {\page-link #82 {\fill-with-pattern #0.2 #LEFT . "Old Dog Tray" \smallCapsOldStyle 79}}
\markup {\page-link #63 {\fill-with-pattern #0.2 #LEFT . "On the Banks of the Wabash, Far Away" \smallCapsOldStyle 60}}
\markup {\page-link #40 {\fill-with-pattern #0.2 #LEFT . "O Sole Mio" \smallCapsOldStyle 37}}
\markup {\page-link #7 {\fill-with-pattern #0.2 #LEFT . "Praise of Spring" \smallCapsOldStyle 4}}
\markup {\page-link #81 {\fill-with-pattern #0.2 #LEFT . "Punchinello" \smallCapsOldStyle 78}}
\markup {\page-link #50 {\fill-with-pattern #0.2 #LEFT . "Red is the Rose" \smallCapsOldStyle 47}}
\markup {\page-link #47 {\fill-with-pattern #0.2 #LEFT . "Robin Adair" \smallCapsOldStyle 44}}
\markup {\page-link #104 {\fill-with-pattern #0.2 #LEFT . "Rule Britannia" \smallCapsOldStyle 101}}
\markup {\page-link #53 {\fill-with-pattern #0.2 #LEFT . "Sailing" \smallCapsOldStyle 50}}
\markup {\page-link #111 {\fill-with-pattern #0.2 #LEFT . "Saint Patrick’s Day" \smallCapsOldStyle 108}}
\markup {\page-link #61 {\fill-with-pattern #0.2 #LEFT . "Santa Lucia" \smallCapsOldStyle 58}}
\markup {\page-link #83 {\fill-with-pattern #0.2 #LEFT . "Saved From the Storm" \smallCapsOldStyle 80}}
\markup {\page-link #67 {\fill-with-pattern #0.2 #LEFT . "Scotch Lassie Jean" \smallCapsOldStyle 64}}
\markup {\page-link #17 {\fill-with-pattern #0.2 #LEFT . "Shoot false love I care not" \smallCapsOldStyle 14}}
\markup {\page-link #22 {\fill-with-pattern #0.2 #LEFT . "Since first I saw your face" \smallCapsOldStyle 19}}
\markup {\page-link #114 {\fill-with-pattern #0.2 #LEFT . "Skating (Round)" \smallCapsOldStyle 111}}
\markup {\page-link #131 {\fill-with-pattern #0.2 #LEFT . "Soldier’s Hymn" \smallCapsOldStyle 128}}
\markup {\page-link #23 {\fill-with-pattern #0.2 #LEFT . "Song of Spring" \smallCapsOldStyle 20}}
\markup {\page-link #86 {\fill-with-pattern #0.2 #LEFT . "Sweet Genevieve" \smallCapsOldStyle 83}}
\markup {\page-link #57 {\fill-with-pattern #0.2 #LEFT . "There’s Music in the Air" \smallCapsOldStyle 54}}
\markup {\page-link #129 {\fill-with-pattern #0.2 #LEFT . "Tit-Willow" \smallCapsOldStyle 126}}
\markup {\page-link #20 {\fill-with-pattern #0.2 #LEFT . "Trust" \smallCapsOldStyle 17}}
\markup {\page-link #45 {\fill-with-pattern #0.2 #LEFT . "Vive L’Amour" \smallCapsOldStyle 42}}
\markup {\page-link #106 {\fill-with-pattern #0.2 #LEFT . "We won’t go home until morning (For he’s a jolly good fellow)" \smallCapsOldStyle 103}}
\markup {\page-link #125 {\fill-with-pattern #0.2 #LEFT . "When I go out of door" \smallCapsOldStyle 122}}
\markup {\page-link #132 {\fill-with-pattern #0.2 #LEFT . "When I in pain and sorrow moan" \smallCapsOldStyle 129}}
\markup {\page-link #131 {\fill-with-pattern #0.2 #LEFT . "When Jesus Wept (Round)" \smallCapsOldStyle 128}}
\markup {\page-link #73 {\fill-with-pattern #0.2 #LEFT . "When You and I Were Young, Maggie" \smallCapsOldStyle 70}}
\markup {\page-link #115 {\fill-with-pattern #0.2 #LEFT . "Where There’s a Will There’s a Way" \smallCapsOldStyle 112}}
\markup {\page-link #27 {\fill-with-pattern #0.2 #LEFT . "With Horse and Hound" \smallCapsOldStyle 24}}
\markup {\page-link #42 {\fill-with-pattern #0.2 #LEFT . "The Ash Grove" \smallCapsOldStyle 39}}
\markup {\page-link #43 {\fill-with-pattern #0.2 #LEFT . "The Battle Hymn of the Republic" \smallCapsOldStyle 40}}
\markup {\page-link #138 {\fill-with-pattern #0.2 #LEFT . "The Battle Prayer" \smallCapsOldStyle 135}}
\markup {\page-link #112 {\fill-with-pattern #0.2 #LEFT . "The bell doth toll (Round)" \smallCapsOldStyle 109}}
\markup {\page-link #90 {\fill-with-pattern #0.2 #LEFT . "The Birds’ Ball" \smallCapsOldStyle 87}}
\markup {\page-link #80 {\fill-with-pattern #0.2 #LEFT . "The Blue Bells of Scotland" \smallCapsOldStyle 77}}
\markup {\page-link #121 {\fill-with-pattern #0.2 #LEFT . "A British Tar" \smallCapsOldStyle 118}}
\markup {\page-link #107 {\fill-with-pattern #0.2 #LEFT . "A Capital Ship" \smallCapsOldStyle 104}}
\markup {\page-link #127 {\fill-with-pattern #0.2 #LEFT . "The criminal cried" \smallCapsOldStyle 124}}
\markup {\page-link #119 {\fill-with-pattern #0.2 #LEFT . "The Distant Shore" \smallCapsOldStyle 116}}
\markup {\page-link #28 {\fill-with-pattern #0.2 #LEFT . "The Flight of Love" \smallCapsOldStyle 25}}
\markup {\page-link #116 {\fill-with-pattern #0.2 #LEFT . "The Flowers that Bloom in the Spring" \smallCapsOldStyle 113}}
\markup {\page-link #74 {\fill-with-pattern #0.2 #LEFT . "The Girl I Left Behind Me" \smallCapsOldStyle 71}}
\markup {\page-link #98 {\fill-with-pattern #0.2 #LEFT . "The Hand that Holds the Bread" \smallCapsOldStyle 95}}
\markup {\page-link #39 {\fill-with-pattern #0.2 #LEFT . "The Harp that Once Through Tara’s Halls" \smallCapsOldStyle 36}}
\markup {\page-link #62 {\fill-with-pattern #0.2 #LEFT . "The Hazel Dell" \smallCapsOldStyle 59}}
\markup {\page-link #34 {\fill-with-pattern #0.2 #LEFT . "The Heart Bowed Down" \smallCapsOldStyle 31}}
\markup {\page-link #63 {\fill-with-pattern #0.2 #LEFT . "A Hot Time in the Old Town" \smallCapsOldStyle 60}}
\markup {\page-link #70 {\fill-with-pattern #0.2 #LEFT . "A Life on the Ocean Wave" \smallCapsOldStyle 67}}
\markup {\page-link #85 {\fill-with-pattern #0.2 #LEFT . "The Little Tin Soldier" \smallCapsOldStyle 82}}
\markup {\page-link #60 {\fill-with-pattern #0.2 #LEFT . "The Lorelei" \smallCapsOldStyle 57}}
\markup {\page-link #101 {\fill-with-pattern #0.2 #LEFT . "The March of Prohibition" \smallCapsOldStyle 98}}
\markup {\page-link #77 {\fill-with-pattern #0.2 #LEFT . "The Midshipmite" \smallCapsOldStyle 74}}
\markup {\page-link #30 {\fill-with-pattern #0.2 #LEFT . "The Minstrel Boy" \smallCapsOldStyle 27}}
\markup {\page-link #92 {\fill-with-pattern #0.2 #LEFT . "The Old Folks at Home" \smallCapsOldStyle 89}}
\markup {\page-link #69 {\fill-with-pattern #0.2 #LEFT . "The Old Musician and His Harp" \smallCapsOldStyle 66}}
\markup {\page-link #58 {\fill-with-pattern #0.2 #LEFT . "The Old Time" \smallCapsOldStyle 55}}
\markup {\page-link #44 {\fill-with-pattern #0.2 #LEFT . "The Roast Beef of Old England" \smallCapsOldStyle 41}}
\markup {\page-link #105 {\fill-with-pattern #0.2 #LEFT . "The Sidewalks of New York" \smallCapsOldStyle 102}}
\markup {\page-link #133 {\fill-with-pattern #0.2 #LEFT . "The Spacious Firmament on High" \smallCapsOldStyle 130}}
\markup {\page-link #109 {\fill-with-pattern #0.2 #LEFT . "The Tailor and the Mouse" \smallCapsOldStyle 106}}
\markup {\page-link #97 {\fill-with-pattern #0.2 #LEFT . "’Twere vain to tell" \smallCapsOldStyle 94}}
\markup {\page-link #10 {\fill-with-pattern #0.2 #LEFT . "The Waits (Let Us Sing)" \smallCapsOldStyle 7}}
\markup {\page-link #103 {\fill-with-pattern #0.2 #LEFT . "A Warrior Bold" \smallCapsOldStyle 100}}