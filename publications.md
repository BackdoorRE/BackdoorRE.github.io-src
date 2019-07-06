---
title: Publications
---

$for(publications)$
<h4>$year$</h4>
<ul>
$for(publications)$
  <li>
    <strong>$title$</strong>$if(pdf)$&nbsp;<a href="$pdf$"><span class="fas fa-file" /></a>$endif$$if(slides)$&nbsp;<a href="$slides$"><span class="fas fa-chalkboard-teacher" /></a>$endif$<br />
    $author$<br />
    $conference$$if(arate)$&nbsp;(<i>acceptance rate: $arate$</i>)$endif$
  </li>
$endfor$
</ul>
$endfor$
