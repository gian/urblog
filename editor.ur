(* Copyright (c) 2008, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * Modified September 2009 by Gian Perrone for use with Urblog from
 * Ur/Web 'Crud' example at:
 * http://www.impredicative.com/ur/demo/
 *)

con colMeta = fn t_formT :: (Type * Type) => {
                 Nam : string,
                 Show : t_formT.1 -> xbody,
                 Widget : nm :: Name -> xml form [] [nm = t_formT.2],
                 WidgetPopulated : nm :: Name -> t_formT.1 -> xml form [] [nm = t_formT.2],
                 Parse : t_formT.2 -> t_formT.1,
                 Inject : sql_injectable t_formT.1
                 }
con colsMeta = fn cols :: {(Type * Type)} => $(map colMeta cols)

style editordefault

fun default [t] (sh : show t) (rd : read t) (inj : sql_injectable t)
            name : colMeta (t, string) =
    {Nam = name,
     Show = txt,
     Widget = fn [nm :: Name] => <xml><textbox{nm} class={editordefault}/></xml>,
     WidgetPopulated = fn [nm :: Name] n =>
                          <xml><textbox{nm} value={show n} class={editordefault}/></xml>,
     Parse = readError,
     Inject = _}

val int = default
val float = default
val string = default
val time = default

fun bool name = {Nam = name,
                 Show = txt,
                 Widget = fn [nm :: Name] => <xml><checkbox{nm}/></xml>,
                 WidgetPopulated = fn [nm :: Name] b =>
                                      <xml><checkbox{nm} checked={b}/></xml>,
                 Parse = fn x => x,
                 Inject = _}

functor Make(M : sig
                 con cols :: {(Type * Type)}
                 constraint [Id] ~ cols
                 val fl : folder cols

                 table tab : ([Id = int] ++ map fst cols)

                 val title : string

                 val cols : colsMeta cols

				 val blogentry : css_class
				 val blogentrytitle : css_class

				 val page : string -> xbody -> transaction page
             end) = struct

    val tab = M.tab
	val page = M.page

    sequence seq

    fun list () =
        rows <- queryX (SELECT * FROM tab AS T)
                       (fn (fs : {T : $([Id = int] ++ map fst M.cols)}) => <xml>
                         <tr>
                           <td>{[fs.T.Id]}</td>
                           {foldRX2 [fst] [colMeta] [tr]
                                    (fn [nm :: Name] [t :: (Type * Type)] [rest :: {(Type * Type)}]
                                                     [[nm] ~ rest] v col => <xml>
                                                       <td>{col.Show v}</td>
                                                     </xml>)
                                    [M.cols] M.fl (fs.T -- #Id) M.cols}
                           <td>
                             <a link={upd fs.T.Id}>[Update]</a>
                             <a link={confirm fs.T.Id}>[Delete]</a>
                           </td>
                         </tr>
                       </xml>);
        return <xml>
		 <div class={M.blogentry}>
		 <div class={M.blogentrytitle}><h2>New Entry</h2></div><br/>
		  <form>
            {foldR [colMeta] [fn cols :: {(Type * Type)} => xml form [] (map snd cols)]
                   (fn [nm :: Name] [t :: (Type * Type)] [rest :: {(Type * Type)}]
                                    [[nm] ~ rest] (col : colMeta t) (acc : xml form [] (map snd rest)) => <xml>
                                      {cdata col.Nam}<br/>{col.Widget [nm]}<br />
                                      {useMore acc}
                                    </xml>)
                     <xml/>
                     [M.cols] M.fl M.cols}
            
            <submit value="Post Entry" action={create}/>
          </form>
		  </div>
		  <br/><hr/><br/>
		  <div class={M.blogentry}>
<div class={M.blogentrytitle}><h2>Entries</h2></div><br/>
          <table border={1}>
            <tr>
              <th>ID</th>
              {foldRX [colMeta] [tr]
                        (fn [nm :: Name] [t :: (Type * Type)] [rest :: {(Type * Type)}]
                                         [[nm] ~ rest] col => <xml>
                                           <th>{cdata col.Nam}</th>
                                         </xml>)
                        [M.cols] M.fl M.cols}
            </tr>
            {rows}
          </table>
		  </div>
          
        </xml>

    and create (inputs : $(map snd M.cols)) =
        id <- nextval seq;
        dml (insert tab
                    (foldR2 [snd] [colMeta]
                            [fn cols => $(map (fn t :: (Type * Type) =>
                                                  sql_exp [] [] [] t.1) cols)]
                            (fn [nm :: Name] [t :: (Type * Type)] [rest :: {(Type * Type)}]
                                             [[nm] ~ rest] =>
                             fn input col acc => acc ++ {nm = @sql_inject col.Inject (col.Parse input)})
                            {} [M.cols] M.fl inputs M.cols
                     ++ {Id = (SQL {[id]})}));
        ls <- list ();
       page "Entry Created" <xml>
          <p>Inserted with ID {[id]}.</p>

          {ls}
        </xml>

    and upd (id : int) =
        let
            fun save (inputs : $(map snd M.cols)) =
                dml (update [map fst M.cols] !
                            (foldR2 [snd] [colMeta]
                                    [fn cols => $(map (fn t :: (Type * Type) =>
                                                          sql_exp [T = [Id = int]
                                                                           ++ map fst M.cols]
                                                                  [] [] t.1) cols)]
                                    (fn [nm :: Name] [t :: (Type * Type)] [rest :: {(Type * Type)}]
                                                     [[nm] ~ rest] =>
                                     fn input col acc => acc ++ {nm =
                                                                 @sql_inject col.Inject (col.Parse input)})
                                    {} [M.cols] M.fl inputs M.cols)
                            tab (WHERE T.Id = {[id]}));
                ls <- list ();
                page "Entry Saved" <xml>
                  <p>Saved!</p>

                  {ls}
                </xml>
        in
            fso <- oneOrNoRows (SELECT tab.{{map fst M.cols}} FROM tab WHERE tab.Id = {[id]});
            case fso : (Basis.option {Tab : $(map fst M.cols)}) of
                None => page "Not Found!" <xml>Not found!</xml>
              | Some fs => page "Edit Entry" <xml>
			  <div class={M.blogentry}>
			  <div class={M.blogentrytitle}><h2>Edit Entry</h2></div><br/>
			  <form>
                {foldR2 [fst] [colMeta] [fn cols :: {(Type * Type)} => xml form [] (map snd cols)]
                        (fn [nm :: Name] [t :: (Type * Type)] [rest :: {(Type * Type)}]
                                         [[nm] ~ rest] (v : t.1) (col : colMeta t)
                                         (acc : xml form [] (map snd rest)) =>
                            <xml>
                              {cdata col.Nam}<br/>{col.WidgetPopulated [nm] v}<br />
                              {useMore acc}
                            </xml>)
                        <xml/>
                        [M.cols] M.fl fs.Tab M.cols}

                <submit action={save}/>
              </form></div></xml>
        end

    and confirm (id : int) =
        let
            fun delete () =
                dml (DELETE FROM tab WHERE Id = {[id]});
                ls <- list ();
                page "Entry Deleted" <xml>
                  <p>Entry Deleted.</p>
                  {ls}
                </xml>
        in
            page "Confirm Entry Deletion" <xml>
              <p>Are you sure you want to delete ID #{[id]}?</p>
              
              <form><submit action={delete} value="I was born sure!"/></form>
            </xml>
        end    

    and editor () =
        ls <- list ();
        page "Blog Administration" <xml>
          {ls}
        </xml>

end
