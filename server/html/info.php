<?php
echo('<html><title>Variables List</title><body>');
echo('If you get no response try to do some request like: <br>/info.php?variable=variablevalue<br><hr>');
$REQUEST=explode('&',$REQUEST);
foreach($REQUEST as $r){
	$r=explode("=",$r);
	$VARIABLE[$r[0]]=$VARIABLE[$r[1]];
	echo("$r[0] = $r[1] <br>");
}

echo('<hr></body></html>');
?>