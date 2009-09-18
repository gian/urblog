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

val btitle = "Test Urblog Blog"

(* Blog Markup:
	text ::= (letters, numbers, whitespace)
	frag ::= <text> 
	frag ::= <text> <newline> <newline>
	frag ::= [b] <text> [/b]
	frag ::= [i] <text> [/i]
	frag ::= [img] <text> [/img]
	doc ::= <frag>*
*)


datatype formatting =
	  Italics of formatting
	| Bold of formatting
	| Image of string 
	| Text of string
	| Para of list formatting 

fun parseMarkup s =
let
	fun match s t = if strlen s < strlen t then False else (String.substring s {Start=0, Len=strlen t}) = t

	fun consume s t = 
		if match s t then String.substring s {Start=strlen t, Len=(strlen s - strlen t)} else "(ERROR)" 
  		
	fun frst s = strsub s 0

	fun rest s = String.substring s {Start = 1, Len=(strlen s) - 1}

	fun text s l = if s = "" || frst s = #"[" || frst s = #"\n" then l else text (rest s) (l ^ (show (frst s)))

	fun frag s = 
		if s = "" then (Text "","") else
		if match s "[b]" then 
		let
			val s' = consume s "[b]"
			val t = text s' ""
			val s'' = consume (consume s' t) "[/b]"
		in
			(Bold (Text t), s'')
		end else
		if match s "[i]" then 
		let
			val s' = consume s "[i]"
			val t = text s' ""
			val s'' = consume (consume s' t) "[/i]"
		in
			(Italics (Text t), s'')
		end else
		if match s "[img]" then 
		let
			val s' = consume s "[img]"
			val t = text s' ""
			val s'' = consume (consume s' t) "[/img]"
		in
			(Image t, s'')
		end else
		let
			val t = text s ""
		in
			(Text t, consume s t)
		end
	
	fun doc s l = 
		if s = "" then (Para l) :: Nil else
		if match s "\n\n" then (Para l) :: doc (consume s "\n\n") Nil 
		else let
			val (f,s') = frag s
		in
			doc s' (List.append l  (f :: Nil))
		end

	fun toXML l = 
		case l of (Italics v) => <xml><i>{toXML v}</i></xml>
		        | (Bold v) => <xml><b>{toXML v}</b></xml>
		        | (Image v) => <xml><img src={bless v}/></xml>
			    | (Text t) => <xml>{cdata t}</xml>
			    | Para l => <xml><p>{List.mapX (fn x => <xml>{toXML x}</xml>) l}</p></xml>
		
in
	<xml>{List.mapX toXML (doc s Nil)}</xml>
end

structure Admin = Editor.Make(
struct
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

and ifAuthed tC fC = 
	us <- getCookie usersession;
	case us of 
		None => return fC
	  | Some (i,p) => (
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
                <div class={blogentrybody}>{parseMarkup row.Blog.Body}</div>
                <div class={blogentrydetail}>
                <div class={blogentryauthor}>Posted by {[row.User.DisplayName]} at {[row.Blog.Created]}</div>
                 <div class={blogentrycomments}>
					<a link={detail row.Blog.Id}>
						{[count]} Comments</a> | <button value="Add Comment" class={commentbutton} onclick={set commentForm row.Blog.Id}/>{eL}
				 </div>
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
	page row.Blog.Title tr

and listing () = 
	queryX' (SELECT * FROM blog, user WHERE blog.Author = user.Id ORDER BY blog.Id DESC)
            (fn row => bentry row)
       
and main () = 
	listn <- listing ();
	lo <- loginForm ();
	page btitle <xml>{listn} {lo}</xml>



