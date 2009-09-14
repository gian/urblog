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
style commentform
style commentbutton

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

fun counter id = r <- oneRow (SELECT COUNT( * ) AS N FROM comment WHERE comment.Parent = {[id]});
		return r.N

fun handler r = return <xml><body>
  <table>
    <tr> <th>Parent:</th> <td>{[r.Parent]}</td> </tr>
    <tr> <th>Name:</th> <td>{[r.AuthorName]}</td> </tr>
    <tr> <th>Comment:</th> <td>{[r.CommentBody]}</td> </tr>
  </table>
</body></xml>

fun mkCommentForm id =
	return <xml><form><hidden{#Parent} value={show id}/>
                    <p>Your Name:<br/></p><textbox{#AuthorName}/><br/>
                    <p>Your Comment:<br/></p><textarea{#CommentBody}/><br/>
                    <submit action={handler}/></form></xml>

(* This function should instantiate an 'Editor' form. *)
fun listing () =
	queryX' (SELECT * FROM blog, user WHERE blog.Author = user.Id ORDER BY blog.Id DESC)
            (fn row => let
		val par = row.Blog.Id
		in
		commentForm <- source 0;
		count <- counter row.Blog.Id;
		return <xml>
 	       	<div class={blogentry}>
       	 	<div class={blogentrytitle}><h2>{[row.Blog.Title]}</h2></div>
        	<div class={blogentrybody}><p>{[row.Blog.Body]}</p></div>
        	<div class={blogentrydetail}>
        	<div class={blogentryauthor}>Posted by {[row.User.DisplayName]} at {[row.Blog.Created]}</div>
        	<div class={blogentrycomments}>{[count]} Comments | <button value="Add Comment" class={commentbutton} onclick={set commentForm par}/></div>
        	</div>
		<div class={commentform}>
			<dyn signal={v <- signal commentForm; 
			 if v > 0 then (mkCommentForm v) else return <xml/>}/></div>
        	</div>
        	</xml> end)
       
fun main () = 
	listn <- listing ();
	 return
		<xml>
                <head>
                <title>Some Blog Title</title>
                <link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
                </head>
                <body>
                <div class={blogcontent}>
                <div class={blogtitle}><h1>Some Blog Title</h1></div>
                {listn}
                </div>
                </body>
                </xml> 


