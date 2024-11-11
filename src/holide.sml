structure HOL_IDE = struct
  val noCompile: PolyML.Compiler.compilerParameters list = []
end;
let (* autoconf-like hack *)
  val s = "structure HOL_IDE = struct\nval noCompile = [PolyML.Compiler.CPNoCompile true]\nend"
  val i = ref 0
  fun read () = (SOME (String.sub (s, !i)) before i := !i + 1) handle Subscript => NONE
  in PolyML.compiler (read, []) () handle _ => () end; (* <- important semicolon *)

structure HOL_IDE: sig

type error =
  {context: PolyML.pretty option, hard: bool, location: PolyML.location, message: PolyML.pretty}

type subtree = PolyML.parseTree option
type trees = PolyML.parseTree list

val prelude: unit -> unit
val postPrelude: unit -> unit

val initialize: string ->
    { compilerOut: string -> unit,
      toplevelOut: string -> unit,
      progress: int -> unit,
      error: error -> unit,
      parseTree: PolyML.parseTree -> unit} -> unit

val moveUp: subtree -> subtree
val moveDown: subtree -> subtree
val moveLeft: subtree -> subtree
val moveRight: subtree -> subtree
val printTree: int -> subtree -> PolyML.pretty option
val navigateTo: subtree -> {startOffset: int, endOffset: int} -> subtree
val navigateTo': trees -> {startOffset: int, endOffset: int} -> subtree

val at: PolyML.parseTree list -> int list -> subtree

datatype built = Built of (int * int) * built list
val build: PolyML.parseTree -> built
val buildList: PolyML.parseTree option -> built list

end =
struct
open HOL_IDE

type error =
  {context: PolyML.pretty option, hard: bool, location: PolyML.location, message: PolyML.pretty}

type subtree = PolyML.parseTree option
type trees = PolyML.parseTree list

fun prelude () = quiet_load := true
fun postPrelude () = (PolyML.print_depth 100; quiet_load := false)

fun initialize text {compilerOut, toplevelOut, progress, error, parseTree} = let
  val qstate = QuoteFilter.UserDeclarations.newstate
    {inscriptp = true, quotefixp = false, scriptfilename = ""}
  val sr = ref text
  val read0 = QuoteFilter.makeLexer (fn _ => !sr before sr := "") qstate
  fun read1 pos = case read0 () of
      (_, "") => (pos, "")
    | (start, tok) => (start - 1, tok)
  fun process (cur as (start, tok)) =
    if tok = "" then (cur, NONE)
    else (cur, SOME (read1 (start + String.size tok)))
  val curToken = ref (process (read1 0))
  val position = ref 0
  fun read2 () = let
    val ((start, s), next) = !curToken
    val i = !position
    in
      if i < String.size s then
        (position := i+1; SOME (String.sub (s, i)))
      else case next of
          NONE => NONE
        | SOME next => (curToken := process next; position := 0; read2 ())
    end
  fun getOffset () = case !curToken of
      ((start, _), NONE) => start
    | ((start, tok), SOME (stop, _)) =>
      if stop = start + String.size tok then start + !position else
      if !position = String.size tok then stop else start
  val errors = ref []
  val serial = ref 1
  val result = ref []
  fun ptFn NONE = ()
    | ptFn (SOME pt) = parseTree pt
  fun codeFn NONE () = ()
    | codeFn (SOME code) () = let
      val {fixes, values, structures, signatures, functors, types} = code ()
      fun enter f = app (f PolyML.globalNameSpace)
      in enter #enterFix fixes; enter #enterType types; enter #enterSig signatures;
         enter #enterStruct structures; enter #enterFunct functors; enter #enterVal values end
  open PolyML.Compiler
  val parameters = noCompile @ [
    CPOutStream compilerOut,
    CPPrintStream toplevelOut,
    CPErrorMessageProc error,
    CPCompilerResultFun (fn (pt, code) => (ptFn pt; codeFn code)),
    CPLineOffset getOffset,
    CPPrintInAlphabeticalOrder false,
    CPBindingSeq (fn () => (fn n => n before serial := n + 1) (!serial))];
  fun loop () = (
    progress (getOffset ());
    if #2 (#1 (!curToken)) = "" then ()
    else (PolyML.compiler (read2, parameters) (); loop ()))
  in loop () end;

fun moveUp NONE = NONE
  | moveUp (SOME (_, props)) = let
    fun find [] = NONE
      | find (PolyML.PTparent p :: _) = SOME (p ())
      | find (_ :: tl) = find tl
    in find props end

fun moveDown NONE = NONE
  | moveDown (SOME (_, props)) = let
    fun find [] = NONE
      | find (PolyML.PTfirstChild p :: _) = SOME (p ())
      | find (_ :: tl) = find tl
    in find props end

fun moveLeft NONE = NONE
  | moveLeft (SOME (_, props)) = let
    fun find [] = NONE
      | find (PolyML.PTpreviousSibling p :: _) = SOME (p ())
      | find (_ :: tl) = find tl
    in find props end

fun moveRight NONE = NONE
  | moveRight (SOME (_, props)) = let
    fun find [] = NONE
      | find (PolyML.PTnextSibling p :: _) = SOME (p ())
      | find (_ :: tl) = find tl
    in find props end

fun printTree _ NONE = NONE
  | printTree n (SOME (_, props)) = let
    fun find [] = NONE
      | find (PolyML.PTprint p :: _) = SOME (p n)
      | find (_ :: tl) = find tl
    in find props end

fun at ls (n::rest) =
    let fun at' [] = I
          | at' (i::rest) = at' rest o funpow i moveRight o moveDown
    in at' rest (SOME (List.nth (ls, n))) end
  | at _ _ = raise Match

datatype built = Built of (int * int) * built list

fun build (tree as ({startPosition, endPosition, ...}, _)) =
  Built ((startPosition, endPosition), buildList (moveDown (SOME tree)))

and buildList NONE = []
  | buildList (tree as SOME t) = build t :: buildList (moveRight tree)

fun navigateTo NONE _ = NONE
  | navigateTo (tree as (SOME ({ startPosition, endPosition, ... }, _)))
               (target as {startOffset, endOffset}) =
    if startOffset >= startPosition andalso endOffset <= endPosition
    then (* It's this node or a child. *)
      case moveDown tree of
        NONE => tree (* No children. *)
      | SOME child => let
        (* See which child it is. *)
        fun findChild (result as ({startPosition, endPosition, ...}, _)) =
          if startOffset >= startPosition andalso endOffset <= endPosition
          then SOME result
          else
            case moveRight (SOME result) of
              NONE => NONE
            | SOME next => findChild next
        in
          case findChild child of
            NONE => tree (* In this *)
          | SOME child => navigateTo (SOME child) target
        end
    else (* Must go out. *)
      navigateTo (moveUp tree) target

fun navigateTo' [] _ = NONE
  | navigateTo' ((tree as ({ startPosition, ... }, _)) :: trees)
                (target as {startOffset, ...}) =
    if startOffset < startPosition
    then navigateTo' trees target
    else navigateTo (SOME tree) target

end;
