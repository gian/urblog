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

val btitle = "Test Blog Title"

datatype formatting =
	  Italics of string
	| Bold of string
	| Image of string
	| Text of string
	| Para of string

fun tokens s =
	let
		(* Work around a compiler bug that prevents passing type constructors around as values *)
		fun cm c z = let
			val x = case z of Italics p => p
			                | Bold p => p
							| Image p => p
							| Text p => p
							| Para p => p

			val (a,b) = case (String.split x c) of None => (x,"") | Some m => m
			val (kw,r) = case (String.split b c) of None => (b,"") | Some m => m 
		in
			if x = "" then Nil else (Text a) :: (Para kw) :: cm c (Text r)
		end
	in
		cm #"_" (Text s)
	end

  			
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

fun counter id = r <- oneRow (SELECT COUNT( * ) AS N FROM comment WHERE comment.Parent = {[id]});
		return r.N

fun page t c =
		aLinks <- ifAuthed <xml><a link={Admin.editor()}>New Entry</a> | <a link={logout()}>Logout</a></xml> <xml/>;
		return <xml>
                <head>
                <title>{[t]} - {[btitle]}</title>
                <link rel="stylesheet" type="text/css" href="http://www.expdev.net/urblog/urblog.css"/>
                </head>
                <body>
				<div class={accountlinks}>{aLinks}</div>
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

and logout () = setCookie usersession (0, "");
				main()

and login r = 
	re' <- oneOrNoRows(SELECT user.Id, user.Username, user.Password FROM user WHERE user.Username = {[r.U]} AND user.Password = {[r.P]});
	case re' of
		None => error <xml>Invalid Login</xml>
	  | Some re => setCookie usersession (re.User.Id, re.User.Password); main ()

and loginForm () =
	 ifAuthed <xml/> <xml><div class={loginbox}><p><b>Login</b></p><form>Username:<br/><textbox{#U}/><br/>
	                   Password:<br/><password{#P}/><br/>
					   <submit value="Login" action={login}/></form></div></xml>

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

and ifAuthed tC fC = 
	us <- getCookie usersession;
	case us of 
			None => return fC
	  	  | (Some (i,p)) => (
		  		l <- oneOrNoRows (SELECT * FROM user WHERE user.Id = {[i]} AND user.Password = {[p]});
				case l of None => return fC
				        | Some x => return tC)

and currentUser () = 1

and editLink n = ifAuthed <xml> | <a link={Admin.upd n}>Edit</a></xml> <xml/>

and bentry row =
	count <- counter row.Blog.Id;
	commentForm <- source 0;
	eL <- editLink row.Blog.Id;
	return <xml>
                <div class={blogentry}>
                <div class={blogentrytitle}><h2><a link={detail row.Blog.Id}>{[row.Blog.Title]}</a></h2></div>
                <div class={blogentrybody}>{List.mapX (fn x => case x of (Italics m) => <xml><i>{[m]}</i></xml>
																	   | (Text m) => <xml>{[m]}</xml>
																	   | (Para m) => <xml><p>{[m]}</p></xml>
																	   | _ => <xml>Invalid blog markup</xml>
											) (tokens row.Blog.Body)}</div>
                <div class={blogentrydetail}>
                <div class={blogentryauthor}>Posted by {[row.User.DisplayName]} at {[row.Blog.Created]}</div>
                <div class={blogentrycomments}><a link={detail row.Blog.Id}>{[count]} Comments</a> | <button value="Add Comment" class={commentbutton} onclick={set commentForm row.Blog.Id}/>{eL}</div>
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



