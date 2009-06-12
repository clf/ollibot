<?PHP
//Filipe Caldas 23/09/2007
//Bind Python server variables inside PHP

$msg="Hello World";
$VARIABLE=array();


$method = getenv("REQUEST_METHOD");
	if($method=="GET"){
		$inc = getenv('INCLUDE');
		$REQUEST=getenv("QUERY_STRING");
		require($inc);
	}elseif($method=="POST"){
		$inc = getenv('INCLUDE');
		$REQUEST=getenv('QUERY_STRING');
		require($inc);
	}else{
	echo('<HTML><TITLE>Error</TITLE><body><h1><hr>Unknow REQUEST_METHOD<hr></h1></body></HTML>');
	}

//This will not be used cause PHP include() don't pass arrays just variables
//		$request=explode('&',$request);
//		foreach($request as $r){
//			$r=explode("=",$r);
//			$VARIABLE[$r[0]]=$VARIABLE[$r[1]];
//		}
//

?>