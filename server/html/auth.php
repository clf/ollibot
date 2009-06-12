<?php
$user='';
$pass='';
$REQUEST=explode('&',$REQUEST);
foreach($REQUEST as $r){
	$r=explode("=",$r);
	if($r[0]=='user'){
		$user=$r[1];
	}
	if($r[0]=='pass'){
		$pass=$r[1];
	}
}


if($user==''){
	echo("<html><body><h1>No try to auth</h1></body></html>");
}else{
	echo("<html><body><h1>Username= $user<br> Password= $pass</h1></body></html>");
}


?>