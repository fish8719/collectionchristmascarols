$regexPageNumFilename = [regex]'^(\d+)[,-]';
$regexPageNum = [regex] '(?<=first-page-number\s+=\s+\#)(\d+)';
#$regex = [regex]'\\lyricmode\s*(?<open>{)(?:(?:(?<quote>[''"])|[^''"{}])*(?(open)((?<-open>})|(?<open>{))|))*(?(open)(?!))(?(quote)|(?!))';
$regex = [regex]'[a-zA-Z]{2,}(?<![a-g][ei]s)''|(?<!#)''[a-zA-Z]|\\lyricmode\s*{[^}]*''';
$files = (ls -filter *.ly);
$version = '\version "2.14.2"';
$include = '\include "../util.ly"';
foreach ($_ in $files) {
  $f = Get-Content $_ -Encoding UTF8;
  $content = $f -replace "OFL Sorts Mill Goudy","GoudyOlSt BT";
  if($content -contains $include) {} else {
    $content[0] = "$include
"+$content[0];
  }
  if($content -contains $version){} else {
    $content[0] = "$version
"+$content[0];
  }
  $match = $regexPageNumFilename.Match($_.Name);
  if($match.Success) {
      $pagenum = $match.Groups[1].Value;
      $match = $regexPageNum.Match($f);
      if($match.Success) {
        $cpagenum = $match.Groups[1].Value;
        if($cpagenum -ne $pagenum) {
          $content = $content -ireplace $regexPageNum, $pagenum;
        }
      } else {
        echo "No page number line found in $_.";
      }
      $match = $regex.Match($f);
      if($match.Success) {
        echo "Neutral quotes found in $_. $match"
      }
   } else {
     echo "No page number found in filename $_.";
   }
   if($content -ne $f) {
     $content | out-file ($_.Name) -Encoding UTF8;
   }
}