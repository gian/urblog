table user : { Id : int, Username : string, Password : string, DisplayName : string }
	PRIMARY KEY Id

table blog : { Id : int, Title : string, BlogBody : string, BlogCreated : time, Public : bool, Author : int}
	PRIMARY KEY Id,
  	CONSTRAINT Author FOREIGN KEY Author REFERENCES user(Id)

table comment : { Id : int, Parent : int, CommentBody : string, CommentCreated : time, AuthorName : string, Key : string }
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


	 structure A = Crud.Make(struct
                   val tab = blog
                             
                   val title = "Blog Administration"
                   val cols = {Title = Crud.string "Title",
                               BlogBody = Crud.string "BlogBody",
                               BlogCreated = Crud.time "BlogCreated",
                               Public = Crud.bool "Public",
			       Author = Crud.int "Author"}
               end)

val admin = A.main

fun page t f d () =
	contents <- f d;
	return <xml>
		<head>
		<title>{[t]}</title>
		<link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
		</head>
		<body>
		<div class={blogcontent}>
		<div class={blogtitle}><h1>{[t]}</h1></div>
		{contents}
		</div>
		</body>
		</xml>
fun entry k =
return <xml>
<div class={blogentry}>
<div class={blogentrytitle}><h2>Test Blog Entry {[k]}</h2></div>
<div class={blogentrybody}><p>This is a test</p></div>
<div class={blogentrydetail}>
<div class={blogentryauthor}>Posted by Gian at 14:21 on 14/09/2009</div>
<div class={blogentrycomments}>0 Comments</div>
</div>
</div>
</xml>
val main = page "foo" entry "Test Title"

