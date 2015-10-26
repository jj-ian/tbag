
type prog =
        | ID of string

type func_decl = {
		(*freturn: string;*)
        fname : string;
        formals : string list;
        (*locals: string list;
        (*body : stmt list;*)
}

(*
let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"
  *)
