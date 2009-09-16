table user : { Id : int, Username : string, Password : string, DisplayName : string }
	PRIMARY KEY Id

table blog : { Id : int, Title : string, Body : string, Created : time, Public : bool, Author : int}
	PRIMARY KEY Id,
  	CONSTRAINT Author FOREIGN KEY Author REFERENCES user(Id)

sequence commentS
table comment : { Id : int, Parent : int, CommentBody : string, CommentCreated : time, AuthorName : string, Key : string }
	PRIMARY KEY Id,
	CONSTRAINT Parent FOREIGN KEY Parent REFERENCES blog(Id)

cookie usersession : int * string

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
style accountlinks
style bodyedit
style commentbox
style loginbox

fun counter id = r <- oneRow (SELECT COUNT( * ) AS N FROM comment WHERE comment.Parent = {[id]});
		return r.N

 
val btitle = "Test Blog Title"

structure Admin = Editor.Make(struct
	val tab = blog
                          
	val title = "Blog Administration"
	
	val cols = {Title = Editor.string "Title",
                Body = {Nam = "Entry Body",
                          Show = (fn b => <xml>{[if strlen b > 25 then substring b 0 25 else b]}...</xml>),
                          Widget = (fn [nm :: Name] => <xml>
                                          <textarea{nm} class={bodyedit}/>
                                        </xml>),
                          WidgetPopulated = (fn [nm :: Name] b => <xml>
                                          <textarea{nm} class={bodyedit}>{[b]}</textarea>
                                        </xml>),
                          Parse = (fn s => readError s),
                          Inject = _
                        },
				Created = Editor.time "Entry Date",
				Public = Editor.bool "Public?",
				Author = {Nam = "",
                          Show = (fn b => <xml/>),
                          Widget = (fn [nm :: Name] => <xml>
                                          <hidden{nm} value={show 1}/>
                                        </xml>),
                          WidgetPopulated = (fn [nm :: Name] b => <xml>
                                          <hidden{nm} value={show b}/>
                                        </xml>),
                          Parse = (fn s => readError s),
                          Inject = _
                        }, 
                }

	val page = fn t c =>
		return <xml>
                <head>
                <title>{[t]} - {[btitle]}</title>
                <link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
                </head>
                <body>
				<div class={accountlinks}>Account Links</div>
                <div class={blogcontent}>
                <div class={blogtitle}><h1>{[btitle]}</h1></div>
                {c}
                </div>
                </body>
                </xml> 



	val blogentrytitle = blogentrytitle
	val blogentry = blogentry

        end)

val admin = Admin.editor

fun page t c =
		return <xml>
                <head>
                <title>{[t]} - {[btitle]}</title>
                <link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
                </head>
                <body>
				<div class={accountlinks}>Account Links</div>
                <div class={blogcontent}>
                <div class={blogtitle}><h1><a link={main ()}>{[btitle]}</a></h1></div>
                {c}
                </div>
                </body>
                </xml> 

and account () =
	pg <- return <xml><h1>Account Settings</h1></xml>;
	pg' <- page "Account Settings" pg;
	return pg'

and login r = return <xml><body><p>Username: {[r.Username]}</p>
								<p>Password: {[r.Password]}</p></body></xml>

and loginForm () =
	 return <xml><div class={loginbox}><p><b>Login</b></p><form>Username:<br/><textbox{#Username}/><br/>
	                   Password:<br/><password{#Password}/><br/>
					   <submit action={login}/></form></div></xml>

and handler r = 
	    id <- nextval commentS;
    		dml (INSERT INTO comment (Id, Parent, AuthorName, CommentBody, CommentCreated, Key)
         	     VALUES ({[id]}, {[readError r.Parent]}, {[r.AuthorName]}, {[r.CommentBody]}, CURRENT_TIMESTAMP, ""));
		(detail (readError r.Parent))

and mkCommentForm id s =
	<xml><form><hidden{#Parent} value={show id}/>
                    <p>Your Name:<br/></p><textbox{#AuthorName}/><br/>
                    <p>Your Comment:<br/></p><textarea{#CommentBody} class={commentbox}/><br/><br/>
                    <submit value="Add Comment" action={handler}/>
		    <button value="Cancel" onclick={set s 0}/></form></xml>

and nl2list s =
  case String.split s #"\n" of
    None => s :: Nil
  | Some (h,t) => h :: nl2list t

and isAuthed () = True

and currentUser () = 1

and editLink n =
(if isAuthed () then <xml> | <a link={Admin.upd n}>Edit</a></xml> else <xml/>)

and bentry row =
	count <- counter row.Blog.Id;
	commentForm <- source 0;
	return <xml>
                <div class={blogentry}>
                <div class={blogentrytitle}><h2><a link={detail row.Blog.Id}>{[row.Blog.Title]}</a></h2></div>
                <div class={blogentrybody}>{List.mapX (fn x => <xml><p>{[x]}</p></xml>) (nl2list row.Blog.Body)}</div>
                <div class={blogentrydetail}>
                <div class={blogentryauthor}>Posted by {[row.User.DisplayName]} at {[row.Blog.Created]}</div>
                <div class={blogentrycomments}><a link={detail row.Blog.Id}>{[count]} Comments</a> | <button value="Add Comment" class={commentbutton} onclick={set commentForm row.Blog.Id}/>{editLink row.Blog.Id}</div>
                </div>
                <div class={commentform}>
                        <dyn signal={v <- signal commentForm;
                         if v > 0 then return (mkCommentForm v commentForm) else return <xml/>}/></div>
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
		tr <- return <xml>{res}<h3>Comments</h3>{com}</xml>;
		p <- page row.Blog.Title tr;
		return p

(* This function should instantiate an 'Editor' form. *)
and listing () = 
	queryX' (SELECT * FROM blog, user WHERE blog.Author = user.Id ORDER BY blog.Id DESC)
            (fn row => bentry row)
       
and main () = 
	listn <- listing ();
	lo <- loginForm ();
	page btitle <xml>{listn} {lo}</xml>



