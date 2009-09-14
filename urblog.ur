table user : { Id : int, Username : string, Password : string, DisplayName : string }
	PRIMARY KEY Id

table blog : { Id : int, Title : string, Body : string, Created : time, Public : bool, Author : int}
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


open Editor.Make(struct
	val tab = blog
                          
	val title = "Blog Administration"
	
	val cols = {Title = Editor.string "Title",
                    Body = Editor.string "Body",
                    Created = Editor.time "Created",
                    Public = Editor.bool "Public",
		    Author = Editor.int "Author"}
        end)

val admin = editor

fun listing () =
	rows <- queryX (SELECT * FROM blog AS Blog ORDER BY blog.Id DESC)
            (fn row =>
		<xml>
 	       	<div class={blogentry}>
       	 	<div class={blogentrytitle}><h2>{[row.Blog.Title]}</h2></div>
        	<div class={blogentrybody}><p>{[row.Blog.Body]}</p></div>
        	<div class={blogentrydetail}>
        	<div class={blogentryauthor}>Posted by row.User.DisplayName at {[row.Blog.Created]}</div>
        	<div class={blogentrycomments}>0 Comments</div>
        	</div>
        	</div>
        	</xml>);
        return
		<xml>
                <head>
                <title>Some Blog Title</title>
                <link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
                </head>
                <body>
                <div class={blogcontent}>
                <div class={blogtitle}><h1>Some Blog Title</h1></div>
                {rows}
                </div>
                </body>
                </xml> 


val main = listing

