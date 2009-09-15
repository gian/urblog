table user : { Id : int, Username : string, Password : string, DisplayName : string }
	PRIMARY KEY Id

table blog : { Id : int, Title : string, Body : string, Created : time, Public : bool, Author : int}
	PRIMARY KEY Id,
  	CONSTRAINT Author FOREIGN KEY Author REFERENCES user(Id)

sequence commentS
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

val btitle = "Test Blog Title"

fun page t c =
		return <xml>
                <head>
                <title>{[t]} - {[btitle]}</title>
                <link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
                </head>
                <body>
                <div class={blogcontent}>
                <div class={blogtitle}><h1><a link={main ()}>{[btitle]}</a></h1></div>
                {c}
                </div>
                </body>
                </xml> 

and handler r = 
	    id <- nextval commentS;
    		dml (INSERT INTO comment (Id, Parent, AuthorName, CommentBody, CommentCreated, Key)
         	     VALUES ({[id]}, {[readError r.Parent]}, {[r.AuthorName]}, {[r.CommentBody]}, {[readError "10/10/10 10:10:10"]}, ""));
		(detail (readError r.Parent))

and mkCommentForm id =
	<xml><form><hidden{#Parent} value={show id}/>
                    <p>Your Name:<br/></p><textbox{#AuthorName}/><br/>
                    <p>Your Comment:<br/></p><textarea{#CommentBody}/><br/><br/>
                    <submit value="Add Comment" action={handler}/></form></xml>

and bentry row = 
	count <- counter row.Blog.Id;
	commentForm <- source 0;
	return <xml>
                <div class={blogentry}>
                <div class={blogentrytitle}><h2><a link={detail row.Blog.Id}>{[row.Blog.Title]}</a></h2></div>
                <div class={blogentrybody}><p>{[row.Blog.Body]}</p></div>
                <div class={blogentrydetail}>
                <div class={blogentryauthor}>Posted by {[row.User.DisplayName]} at {[row.Blog.Created]}</div>
                <div class={blogentrycomments}><a link={detail row.Blog.Id}>{[count]} Comments</a> | <button value="Add Comment" class={commentbutton} onclick={set commentForm row.Blog.Id}/></div>
                </div>
                <div class={commentform}>
                        <dyn signal={v <- signal commentForm;
                         if v > 0 then return (mkCommentForm v) else return <xml/>}/></div>
                </div>
                </xml>

and detail id = row <- oneRow (SELECT * FROM blog, user WHERE blog.Author = user.Id AND blog.Id = {[id]});
		res <- bentry row;
		com <- queryX (SELECT * FROM comment WHERE comment.Parent = {[id]})
			(fn r => <xml>
			                <div class={blogentrybody}><p>{[r.Comment.CommentBody]}</p></div>
        			        <div class={blogentrydetail}>
        			        <div class={blogentryauthor}>Posted by {[r.Comment.AuthorName]} at {[r.Comment.CommentCreated]}</div>
				</div></xml>);
		tr <- return <xml>{res}{com}</xml>;
		p <- page row.Blog.Title tr;
		return p

(* This function should instantiate an 'Editor' form. *)
and listing () = 
	queryX' (SELECT * FROM blog, user WHERE blog.Author = user.Id ORDER BY blog.Id DESC)
            (fn row => bentry row)
       
and main () = 
	listn <- listing ();
	p <- page btitle listn;
	return p


