table user : { Id : int, Username : string, Password : string, DisplayName : string }
	PRIMARY KEY Id

table blog : { Id : int, Title : string, Body : string, Created : time, Public : bool, Author : int}
	PRIMARY KEY Id,
  	CONSTRAINT Author FOREIGN KEY Author REFERENCES user(Id)

table comment : { Id : int, Parent : int, Body : string, Created : time, AuthorName : string, Key : string }
	PRIMARY KEY Id,
	CONSTRAINT Parent FOREIGN KEY Parent REFERENCES blog(Id)

style blogentry
style blogentrytitle
style blogentrydetail
style blogentrybody
style blogentryauthor
style blogentrycomments
style blogcontent
style blogtitle

fun entry () =
return <xml>
<div class={blogentry}>
<div class={blogentrytitle}><h2>Test Blog Entry</h2></div>
<div class={blogentrybody}><p>This is a test.<br/><br/>Yes, a test.</p><br /><br /><br /></div>
<div class={blogentrydetail}>
<div class={blogentryauthor}>Posted by Gian at 14:21 on 14/09/2009</div>
<div class={blogentrycomments}>0 Comments</div>
</div>
</div>
</xml>

fun main () =
	xml <- entry ();
	return <xml>
		<head>
		<title>Urblog Test</title>
		<link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
		</head>
		<body>
		<div class={blogcontent}>
		<div class={blogtitle}><h1>Urblog Test</h1></div>
			{xml}
		</div>
		</body>
		</xml>


