  let process_str (str : string) =
    List.filter (fun x -> x <> "") 
    (String.split_on_char ' ' (String.fold_left (fun acc x ->
      if(x != '\n' && x != '\t' && x != '\012')
        then acc ^ String.make 1 x else acc ^ " ")
    "" str))
  
  (* process_str "Push 2\n    Push \"test\"\nPush 4\nPush 6\nMul\nQuit";; *)
  
  type 'a option =
    | None
    | Some of 'a
  type const = Int of int | Str of string | Name of string | Left of const | Right of const | Tup of const list
  | Clo of const * const * cmd list * ((const * const) list) | Empty
  and cmd = Push of const | Pop | Add | Sub | Mul | Div | Swap | Neg | Concat | Quit | Err | And | Or | Not | Equal
  | Lte | Local of const | Global of const | BeginEnd of cmd list | Hold | Quit2 | IfThen of cmd list | Else of cmd list
  | Quit3 | IfThenElse of cmd list * cmd list | InjL | InjR | CaseLeftRight of cmd list * cmd list | Hold2 | Tuple of const 
  | Tuple2 of const list| Get of const | Fun of func list | Call | Return
  and func = (const * const * cmd list)
  type local = (const * const) list
  type global = (const * const) list
  
  let int_checker (x : string) =
    if((String.fold_left(fun acc x ->
    (match x with
    | '0' -> acc
    | '1' -> acc
    | '2' -> acc
    | '3' -> acc
    | '4' -> acc
    | '5' -> acc
    | '6' -> acc
    | '7' -> acc
    | '8' -> acc
    | '9' -> acc
    | '-' -> acc
    | _ -> acc + 1))
    0 x) > 0) then false
    else true
  
    (* int_checker "2424";; *)
    (* int_checker "24e4";; *)
  let name_checker (x : string) =
    if((String.fold_left(fun acc x ->
      (match x with
      | '\"' -> acc + 1
      | _ -> acc))
      0 x) == 2) then false
      else true
  
    (* name_checker "\"Hello\"";; *)
    (* name_checker "Hello";; *)
  
  let strip1 (lis : cmd list option) =
    match lis with
    | Some(x) -> x
    | None -> [Err]
    
  (* strip1 (change_to_prog (process_str "Push 2\nPush \"test\"\nPush 4\nPush 6\nMul\nQuit"));; *)
    
  let strip2 (lis : const list option) =
    match lis with
    | Some(x) -> x
    | None -> []
    
  (* strip2 (eval (strip1 (change_to_prog (process_str "Push 2\nPush \"test\"\nPush 4\nPush 6\nMul\nQuit"))) []);; *)
  
  let beginend_helper (lis : string list) = 
    let x, y, z, e = List.fold_left(fun (acc, count, stop, ends) hd ->
      if(hd = "Begin") then
        (hd :: acc, count, stop + 1, ends)
      else if (hd = "End" && count < stop && ends == stop) then
        (acc, count + 1, stop, ends)
      else if (hd = "End" && count < stop) then
        (hd :: acc, count + 1, stop, ends + 1)
      else if (count < stop) then
        (hd :: acc, count, stop, ends)
      else
        (acc, count, stop, ends)
    ) ([], 0, 1, 1) lis
    in List.rev("Quit" :: x)
  
  (* beginend_helper ["aa"; "bb"; "End"; "test"];; *)
  (* beginend_helper ["aa"; "Begin"; "bb"; "End"; "cc"; "End"; "test"];; *)
  (* beginend_helper ["aa"; "Begin"; "bb"; "Begin"; "cc"; "End"; "dd"; "End"; "ee"; "End"; "test"];; *)
  (* beginend_helper ["Push"; "5"; "Sub"; "End"; "Quit"];; *)

  (* beginend_helper (process_str "IfThen\nPush 6\nElse\nPush 7\nEnd\nEnd\nQuit");; *)
  let hold_counter (lis : cmd list) =
    (List.fold_left(fun count hd -> if(hd == Hold) then count + 1 else count) 0 lis)
  
  let tail (lis : cmd list) =
    let x, y = List.fold_left(fun (acc, count) hd ->
      (match hd with
      (* | IfThenElse(x, y) -> (acc, count + 1) *)
      | Hold -> if(count == hold_counter lis) then
          (acc, count)
        else
          (acc, count + 1)
      | _ -> if(count >= hold_counter lis) then
        (hd :: acc, count)
        else
          (acc, count))
    ) ([], 0) lis
    in List.rev x


    (* | IfThenElse(x, y) -> (acc, count + 1) *)
  
  (* tail [Push(Int 7); Push(Int 4); Hold; Push(Int 5); Hold; Push(Int 6); Quit];; *)
  (* tail [Push(Int 2); BeginEnd [Push (Int 3); Push (Int 4); Quit]; Push (Int 3); Push(Int 4); Hold; Push(Int 5); Hold; Push (Int 6); Quit];; *)
  (* tail [Push (Int 5); Sub; Hold; Push (Int 3); Add; Quit];; *)

  (* tail [IfThenElse ([Push (Int 6); Quit], [Push (Int 7); Quit]); Quit];; *)
  
  let beginend_counter (lis : cmd list) =
    (List.fold_left(fun count hd -> match hd with
    | BeginEnd(x) -> count + 1
    | _ -> count) 0 lis)
  
  (* beginend_counter [Push(Int 2); BeginEnd [Push (Int 3); Push (Int 4); Quit]; Push(Int 6); Quit];; *)
  
    let tail2 (lis : cmd list) =
      let x, y = List.fold_left(fun (acc, count) hd ->
        match hd with
        | BeginEnd(x) -> (acc, count + 1)
        | _ -> if (count >= beginend_counter lis) then (hd :: acc, count) else (acc, count)
      ) ([], 0) lis
      in List.rev x
  
  (* tail2 [Push(Int 2); BeginEnd [Push (Int 3); BeginEnd [Push (Int 3); Push (Int 4); Quit]; Push (Int 4); Quit]; Push(Int 6); Quit];; *)
  
  let ifthen_helper (lis : string list) = 
    let x, y, z, e = List.fold_left(fun (acc, count, stop, elses) hd ->
      if(hd = "IfThen") then
        (hd :: acc, count, stop + 1, elses)
      else if (hd = "Else" && count < stop && elses == stop) then
        (acc, count + 1, stop, elses)
      else if (hd = "Else" && count < stop) then
        (hd :: acc, count + 1, stop, elses + 1)
      else if (count < stop) then
        (hd :: acc, count, stop, elses)
      else
        (acc, count, stop, elses)
    ) ([], 0, 1, 1) lis
    in List.rev("Quit" :: x)
  
  (* ifthen_helper ["Push"; "5"; "Add"; "Else"; "Push"; "5"; "Sub"; "End"; "Quit"];; *)
  (* ifthen_helper ["Push"; "\"True\""; "Else"; "Push"; "\"False\""; "End"; "Quit"];; *)
let else_helper (lis : string list) =
  let x, y, z, w  = List.fold_left(fun (acc, start, finish, end_counter) hd ->
    (match start with
    | true ->
      (match finish with
      | true -> (acc, start, finish, end_counter)
      | false -> (match hd with
        | "End" -> 
          if(end_counter == 0) then
            (acc, start, true, end_counter)
          else
            (hd :: acc, start, finish, end_counter + 1)
        | "Begin" -> (hd :: acc, start, finish, end_counter - 1)
        | _ -> (hd :: acc, start, finish, end_counter)))
    | false ->
        match hd with
        | "Else" -> (acc, true, finish, end_counter)
        | _ -> (acc, start, finish, end_counter))
    ) ([], false, false, 0) lis
  in List.rev ("Quit" :: x)

(* else_helper (process_str "Pop\nElse\nBegin\nPush 5\nEnd\nAdd\nEnd\nLte\nQuit");; *)

  let ifthen_tail (lis : cmd list) =
    let x, y = List.fold_left(fun (acc, count) hd ->
      match hd with
      | Hold -> (acc, count + 1)
      | BeginEnd(x) -> (acc, count + 2)
      | _ -> if (count > 1) then
        (hd :: acc, count)
        else (acc, count)
    ) ([], 0) lis
    in List.rev x
  
  (* ifthen_tail [Push (Int 5); Sub; Hold; Quit];; *)
  (* ifthen_tail [Push (Int 5); Add; Hold; Push (Int 3); Quit];; *)
  (* ifthen_tail [Push (String "\"True\""); Hold; Push (String "\"False\""); Hold; Quit];; *)
  (* ifthen_tail (strip1(change_to_prog(process_str "Begin\nPush 1\nEnd\nElse\nPush 2\nEnd\nQuit")));; SHOULD PRINT QUIT*)
    (* tail(strip1(change_to_prog (process_str "Else\nPush 2\nEnd\nQuit")));; *)

  (* tail (strip1(change_to_prog(process_str "Push 1\nEnd\nElse\nPush 2\nEnd\nQuit")));; *)
  (* THE ISSUE IS THAT THERE IS A HOLD AT THE VERY END  *)

  let caseleft_helper (lis : string list) = 
    let x, y, z, e = List.fold_left(fun (acc, count, stop, elses) hd ->
      if(hd = "CaseLeft") then
        (hd :: acc, count, stop + 1, elses)
      else if (hd = "Right" && count < stop && elses == stop) then
        (acc, count + 1, stop, elses)
      else if (hd = "Right" && count < stop) then
        (hd :: acc, count + 1, stop, elses + 1)
      else if (count < stop) then
        (hd :: acc, count, stop, elses)
      else
        (acc, count, stop, elses)
    ) ([], 0, 1, 1) lis
    in List.rev("Quit" :: x)
  
  (* ifthen_helper ["Push"; "5"; "Add"; "Else"; "Push"; "5"; "Sub"; "End"; "Quit"];; *)
  (* ifthen_helper ["Push"; "\"True\""; "Else"; "Push"; "\"False\""; "End"; "Quit"];; *)

    let right_helper (lis : string list) =
      let x, y, z, w, e  = List.fold_left(fun (acc, start, finish, right_counter, end_counter) hd ->
        (match start with
        | true ->
          (match finish with
          | true -> (acc, start, finish, right_counter, end_counter)
          | false -> (match hd with
            | "End" -> 
              if(end_counter == 0) then
                (acc, start, true, right_counter, end_counter)
              else
                (hd :: acc, start, finish, right_counter, end_counter + 1)
            | "CaseLeft" -> (hd :: acc, start, finish, right_counter, end_counter - 1)
            | _ -> (hd :: acc, start, finish, right_counter, end_counter)))
        | false ->
            match hd with
            | "Right" -> 
              if(right_counter == 0) then
                (acc, true, finish, right_counter, end_counter)
              else
                (acc, start, finish, right_counter + 1, end_counter)
            | "CaseLeft" -> (acc, start, finish, right_counter - 1, end_counter)
            | _ -> (acc, start, finish, right_counter, end_counter))
        ) ([], false, false, 0, 0) lis
      in List.rev ("Quit" :: x)
(* right_helper (process_str "Push 1\nPush 4\nRight\nAdd\nCaseLeft\nPush 2\nRight\nPush 3\nEnd\nPush 5\nEnd\nQuit");; *)
(* right_helper (process_str "Push 1\nPush 4\nRight\nAdd\nEnd\nQuit");; *)
  
let obtain_top (lis : const list) =
    match lis with
    | x :: tl -> x
    | _ -> Str("Error")

let obtain_top2 (lis : cmd list) =
    match lis with
    | [] -> Err
    | x :: tl -> x

(* let isQuit (lis : string list) =
    List.fold_left (fun quit hd ->
      match hd with
      | "Quit" -> true
      | _ -> quit
      ) false lis *)

  let last_elem (lis : cmd list) =
    obtain_top2 (List.rev lis)
  
(* last_elem [Add; Sub; Hold; Quit];; *)

let leftright_tail (lis : string list) =
  let x, y, z = List.fold_left(fun (acc, start, end_counter) hd ->
    (match start with
    | true -> (hd :: acc, start, end_counter)
    | false ->
        (match hd with
        | "End" -> 
          if(end_counter == 0) then
            (acc, true, end_counter)
          else
            (acc, start, end_counter + 1)
        | "CaseLeft" -> (acc, start, end_counter - 1)
        | _ -> (acc, start, end_counter)))
    ) ([], false, 0) lis
  in List.rev x

(* leftright_tail (process_str "Pop\nPush \"IncorrectBranch\"\nCaseLeft\nPush \"Error1\"\nRight\nPush \"Error2\"\nEnd\nRight\nPush \"right\"\nPush \"case\"\nConcat\nSwap\nCaseLeft\nPush \"foo\"\nRight\nPush \"rightAgain\"\nSwap\nEnd\nPush \"complete\"\nEnd\nAdd\nQuit");; *)
  
let mut_counter (lis : string list) =
  List.fold_left(fun count hd -> 
    match hd with
    | "Mut" -> count + 1
    | _ -> count
    ) 0 lis

let fun_helper (lis : string list) = 
  let x, y, z, e = List.fold_left(fun (acc, count, stop, end_counter) hd ->
    if(stop = false) then
      (if(count == 0 || count == 1) then
        (acc, count + 1, stop, end_counter)
      else
        match hd with
        | "Mut" -> (acc, count + 1, true, end_counter)
        | "End" -> 
          if(end_counter == 0) then
            (acc, count + 1, true, end_counter)
          else
            (hd :: acc, count + 1, stop, end_counter + 1)
        | "IfThen" -> (hd :: acc, count + 1, stop, end_counter - 1)
        | _ -> (hd :: acc, count + 1, stop, end_counter)
      )
    else
      (acc, count + 1, stop, end_counter)
  ) ([], 0, false, 0) lis
  in List.rev ("Quit" :: x)
let name_getter (lis : string list) =
  match lis with
  | x :: tl -> Name(x)
  | _ -> Str("Error")
let arg_getter (lis : string list) =
  match lis with
  | x :: y :: tl -> Name(y)
  | _ -> Str("Error")
let fun_tail (lis : string list) = 
  let x, y = List.fold_left (fun (acc, start) hd ->
    if(start = true) then
      (hd :: acc, start)
    else
      (match hd with
      | "Mut" -> (acc, true)
      | _ -> (acc, start)
      )
  ) ([], false) lis
  in (List.rev x)

(* fun_helper ["isOdd"; "x"; "Push"; "5"; "Mut"; "isEven"; "y"; "Push"; "7"; "End"; "Quit"];; *)
(* mut_counter ["Fun"; "isOdd"; "x"; "Push"; "5"; "Mut"; "isEven"; "y"; "Push"; "7"; "End"; "Quit"];; *)

(* func_list ("Mut" :: ["isOdd"; "x"; "Push"; "5"; "Mut"; "isEven"; "y"; "Push"; "7"; "End"; "Quit"]);; *)
(* func_list ("Mut" :: ["isOdd"; "x"; "Push"; "5"; "Mut"; "isEven"; "y"; "Push"; "7"; "Mut"; "isSome"; "w"; "Push"; "8"; "End"; "Quit"]);; *)

(* get string list of everything before end *)
(* count occurences of mut *)

let fun_final_tail (lis : string list) =
  let x, y, z = List.fold_left (fun (acc, start, end_counter) hd ->
    if(start = true) then
      (hd :: acc, start, end_counter)
    else
      (match hd with
        | "End" -> 
          if(end_counter == 0) then
            (acc, true, end_counter)
          else
            (acc, start, end_counter + 1)
        | "IfThen" -> (acc, start, end_counter - 1)
        | _ -> (acc, start, end_counter))
  ) ([], false, 0) lis
  in (List.rev x)

(* fun_final_tail ["isOdd"; "x"; "Push"; "5"; "Mut"; "isEven"; "y"; "Push"; "7"; "End"; "And"; "Quit"];; *)
(* fun_final_tail ["odd"; "x"; "Push"; "x"; "Push"; "2"; "Mul"; "Local"; "x"; "Push"; "x";
 "Push"; "46"; "Equal"; "IfThen"; "Push"; "x"; "Return"; "Else"; "Push"; "x";
 "Push"; "even"; "Call"; "Return"; "End"; "Mut"; "even"; "x"; "Push"; "1";
 "Push"; "x"; "Add"; "Local"; "x"; "Push"; "x"; "Push"; "odd"; "Call";
 "Return"; "End"; "Push"; "5"; "Push"; "odd"; "Quit"];; *)
      
  let rec change_to_prog (lis : string list) = 
    match lis with
    | "Pop" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Pop :: x)
      | None -> None)
    | "Push" :: const :: t ->
      (match (change_to_prog t) with
      | Some x -> 
        if(int_checker const)
          then (Some(Push(Int(int_of_string const)) :: x))
        else
          if(name_checker const) then
            (Some(Push(Name(const)) :: x))
          else
            (Some(Push(Str(const)) :: x))
      | None -> None)
    | "Add" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Add :: x)
      | None -> None)
    | "Sub" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Sub :: x)
      | None -> None)
    | "Mul" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Mul :: x)
      | None -> None)
    | "Div" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Div :: x)
      | None -> None)
    | "Swap" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Swap :: x)
      | None -> None)
    | "Neg" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Neg :: x)
      | None -> None)
    | "Concat" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Concat :: x)
      | None -> None)
    | "And" :: t -> (match (change_to_prog t) with
      | Some x -> Some(And :: x)
      | None -> None)
    | "Or" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Or :: x)
      | None -> None)
    | "Not" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Not :: x)
      | None -> None)
    | "Equal" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Equal :: x)
      | None -> None)
    | "Lte" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Lte :: x)
      | None -> None)
    | "Local" :: const :: t -> (match (change_to_prog t) with
      | Some x -> 
        if(int_checker const)
          then (Some(Local(Int(int_of_string const)) :: x))
        else
          if(name_checker const) then
            (Some(Local(Name(const)) :: x))
          else
            (Some(Local(Str(const)) :: x))
      | None -> None)
    | "Global" :: const :: t -> (match (change_to_prog t) with
      | Some x -> 
        if(int_checker const)
          then (Some(Global(Int(int_of_string const)) :: x))
        else
          if(name_checker const) then
            (Some(Global(Name(const)) :: x))
          else
            (Some(Global(Str(const)) :: x))
      | None -> None)
    | "Begin" :: t -> (match (change_to_prog t) with
      | Some x -> Some(BeginEnd (strip1 (change_to_prog(beginend_helper t))) :: tail2(tail x))
      | None -> None)
    | "End" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Hold :: x)
      | None -> None)
    | "IfThen" :: t -> (match (change_to_prog t) with
      | Some x -> Some(IfThenElse ((strip1 (change_to_prog(ifthen_helper t))), (strip1 (change_to_prog(else_helper t)))) :: ifthen_tail x)
      | None -> None)
    | "Else" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Hold :: x)
      | None -> None)
    | "CaseLeft" :: t -> Some(CaseLeftRight ((strip1 (change_to_prog(caseleft_helper t))), (strip1 (change_to_prog(right_helper t)))) :: strip1 (change_to_prog (leftright_tail t)))
    | "Right" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Hold2 :: x)
      | None -> None)
    | "InjL" :: t -> (match (change_to_prog t) with
      | Some x -> Some(InjL :: x)
      | None -> None)
    | "InjR" :: t -> (match (change_to_prog t) with
      | Some x -> Some(InjR :: x)
      | None -> None)
    | "Fun" :: t -> Some((Fun (func_list ("Mut" :: t))) :: strip1(change_to_prog (fun_final_tail t)))
    | "Tuple" :: const :: t ->
      (match (change_to_prog t) with
      | Some x -> 
        if(int_checker const)
          then (Some(Tuple(Int(int_of_string const)) :: x))
        else if(name_checker const) then
          (Some(Push(Name(const)) :: x))
        else
          None
      | None -> None)
    | "Get" :: const :: t ->
      (match (change_to_prog t) with
      | Some x -> 
        if(int_checker const)
          then (Some(Get(Int(int_of_string const)) :: x))
        else if(name_checker const) then
          (Some(Push(Name(const)) :: x))
        else
          None
      | None -> None)
    | "Call" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Call :: x)
      | None -> None)
    | "Return" :: t -> (match (change_to_prog t) with
      | Some x -> Some(Return :: x)
      | None -> None)
    | "Quit" :: t -> Some(Quit :: [])
    | _ -> None

and func_list (lis : string list) =
  match lis with
  | "Mut" :: tl -> ((name_getter tl), (arg_getter tl), strip1 (change_to_prog (fun_helper tl))) :: func_list tl
  | _ :: tl -> func_list tl
  | _ -> []

(* change_to_prog (process_str "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 46\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nReturn\nEnd\nEnd\nPush 5\nPush odd\nCall\nQuit");; *)
(* fun_helper ["odd"; "x"; "Push"; "x"; "Push"; "2"; "Mul"; "Local"; "x"; "Push"; "x"; "Push"; "46"; "Equal"; "IfThen"; "Push"; "x"; "Return"; "Else"; "Push"; "x"; "Return"; "End"; "End"; "Push"; "5"; "Push"; "odd"; "Call"; "Quit"];; *)

  (* change_to_prog ["Push"; "10"; "Push"; "1"; "IfThen"; "Push"; "5"; "Add"; "Else"; "Push"; "5"; "Sub"; "End"; "Push"; "3"; "Quit"];; *)
  (* change_to_prog (process_str "Push 1\nLocal one\nBegin\nPush 2\nLocal two\nBegin\nPush one\nPush two\nSub\nEnd\nEnd\nIfThen\nPush \"True\"\nElse\nPush \"False\"\nEnd\nPush 2\nQuit");; *)
  (* change_to_prog (process_str "Push 5\nPush 1\nIfThen\nBegin\nPush 1\nEnd\nElse\nPush 2\nEnd\nQuit");; *)
  (* change_to_prog (process_str "Begin\nPush 1\nIfThen\nPush 6\nElse\nPush 7\nEnd\nEnd\nQuit");; *)

(* change_to_prog (process_str "Push 1\nPush \"two\"\nPush 3\nTuple 3\nQuit");; *)
(* change_to_prog (process_str "Push 10\nInjL\nCaseLeft\nPush 1\nCaseLeft\nPush 2\nRight\nPush 3\nEnd\nPush 4\nRight\nEnd\nAdd\nQuit");; *)
(* right_helper (process_str "Push 10\nInjL\nCaseLeft\nPush 1\nCaseLeft\nPush 2\nRight\nPush 3\nEnd\nPush 4\nRight\nEnd\nQuit");; *)

(* change_to_prog (process_str "Push 25\nPush 2\nNeg\nMul\nPush 0\nLte\nIfThen\nBegin\nPush \"pepper\"\nPush \"onion\"\nPush \"garlic\"\nPush \"carrot\"\nTuple 4\nEnd\nGlobal veggie\nPush veggie\nLocal mystery\nElse\nBegin\nPush \"orange\"\nPush \"melon\"\nPush \"apple\"\nPush \"pear\"\nPush \"grape\"\nPush \"peach\"\nTuple 5\nEnd\nGlobal fruit\nPush fruit\nLocal mystery\nEnd\nPush mystery\nQuit");; *)
(* change_to_prog (process_str "Push 0\nIfThen\nElse\nBegin\nPush 5\nEnd\nAdd\nEnd\nLte\nQuit");; *)

(* change_to_prog ["Fun"; "isOdd"; "x"; "Push"; "5"; "Mut"; "isEven"; "y"; "Push"; "7"; "Mut"; "isSome"; "w"; "Push"; "8"; "End"; "Quit"];; *)
  
  
  let push (x : const) (lis : const list) =
    x :: lis
  let pop (lis : const list) = 
    if (List.length lis < 1) then (Str("Error") :: lis) else
    let x, y = List.fold_left(fun (acc, first) hd ->
      if (first == true)
        then (acc, false)
      else
        (hd :: acc, false)
      )
      ([], true) lis
    in List.rev x
  let add (lis : const list) =
    match lis with
    | Int(a) :: Int(b) :: tl -> Int(a + b) :: tl
    | _ -> Str("Error") :: []
  let sub (lis : const list) =
    match lis with
    | Int(a) :: Int(b) :: tl -> Int(a - b) :: tl
    | _ -> Str("Error") :: []
  let mul (lis : const list) =
    match lis with
    | Int(a) :: Int(b) :: tl -> Int(a * b) :: tl
    | _ -> Str("Error") :: []
  let div (lis : const list) =
    match lis with
    | Int(a) :: Int(0) :: tl -> Str("Error") :: []
    | Int(a) :: Int(b) :: tl -> Int(a / b) :: tl
    | _ -> Str("Error") :: []
  let swap (lis : const list) =
    match lis with
    | x :: y :: tl -> y :: x :: tl
    | _ -> Str("Error") :: []
  let neg (lis : const list) =
    match lis with
    | Int(a) :: tl -> Int(-1 * a) :: tl
    | _ -> Str("Error") :: []
  let correct_quot (str : string) =
    (String.fold_left (fun acc x ->
      if(x != '\"')
        then acc ^ String.make 1 x else acc)
    "\"" str) ^ "\""
  let concat (lis : const list) =
    match lis with
    | Str(a) :: Str(b) :: tl -> Str(correct_quot(a ^ b)) :: tl
    | _ -> Str("Error") :: []
  let andCmd (lis : const list) = 
    match lis with
    | Int(0) :: Int(0) :: tl -> Int(0) :: tl
    | Int(0) :: Int(1) :: tl -> Int(0) :: tl
    | Int(1) :: Int(0) :: tl -> Int(0) :: tl
    | Int(1) :: Int(1) :: tl -> Int(1) :: tl
    | _ -> Str("Error") :: []
  let orCmd (lis : const list) = 
    match lis with
    | Int(0) :: Int(0) :: tl -> Int(0) :: tl
    | Int(0) :: Int(1) :: tl -> Int(1) :: tl
    | Int(1) :: Int(0) :: tl -> Int(1) :: tl
    | Int(1) :: Int(1) :: tl -> Int(1) :: tl
    | _ -> Str("Error") :: []
  let not (lis : const list) = 
    match lis with
    | Int(0) :: tl -> Int(1) :: tl
    | Int(1) :: tl -> Int(0) :: tl
    | _ -> Str("Error") :: []
  let equal (lis : const list) = 
    match lis with
    | Int(a) :: Int(b) :: tl -> if(a == b) then Int(1) :: tl else Int(0) :: tl
    | _ -> Str("Error") :: []
let lte (lis : const list) = 
  match lis with
  | Int(a) :: Int(b) :: tl -> if(a <= b) then Int(1) :: tl else Int(0) :: tl
  | _ -> Str("Error") :: []

let tuple (length : const) (lis : const list) = 
  let x, y = List.fold_left (fun (acc, count) hd ->
    if(count < match length with | Int(a) -> a | _ -> 0) then (hd :: acc, count + 1) else (acc, count + 1)) ([], 0) lis
  in x

  (* tuple (Int(2)) [(Int(3)); (Int(1)); (Int(4)); (Int(6))];; *)

let tuple_remain (length : const) (lis : const list) =
  let x, y = List.fold_left(fun (acc, count) hd ->
    if(count >= match length with | Int(a) -> a | _ -> 0) then
      (hd :: acc, count + 1)
    else
      (acc, count + 1)
  ) ([], 0) lis
  in List.rev x

  (* tuple_remain (Int(2)) [(Int(3)); (Int(1)); (Int(4)); (Int(6))];; *)

  let injL (lis : const list) =
    match lis with
    | [] -> [Str "Error"]
    | _ -> Left(obtain_top lis) :: pop(lis)

  (* injL [(Int(2)); (Int(3)); (Int(4)); (Int(6))];; *)
  (* injL [];; *)

  let injR (lis : const list) =
    match lis with
    | [] -> [Str "Error"]
    | _ -> Right(obtain_top lis) :: pop(lis)

  (* injR [Left (Int 2); Int 3; Int 4; Int 6];; *)
  
  let rec global_finder (x : const) (lis : global) =
    match lis with
    | [] -> Str("Error")
    | (a, b) :: tl -> if(x = a) then b else global_finder (x) (tl)
  
  let rec local_finder (x : const) (l : local) (g : global) =
    match l with
    | [] -> global_finder (x) (g)
    | (a, b) :: tl -> if(x = a) then b else local_finder (x) (tl) (g)
  
  (* local_finder (Name("x"))([(Name("x"), Int(1)); (Name("y"), Int(2))]);; *)
  (* local_finder (Name("y"))([(Name("x"), Int(1)); (Name("y"), Int(2))]);; *)
  
  (* local_finder (Name("z"))([(Name("x"), Int(1)); (Name("y"), Int(2))]) ([(Name("z"), Int(3)); (Name("y"), Int(4))]);; *)
  
  let get_head (lis : const list) =
    match lis with
    | x :: t -> x
    | _ -> Str("Error")
  
  let replace_quit_g (lis : cmd list) =
    List.rev(Quit2 :: List.fold_left(fun acc hd -> if(hd = Quit) then acc else hd :: acc) [] lis)
  
  let replace_quit_l (lis : cmd list) =
    List.rev(Quit3 :: List.fold_left(fun acc hd -> if(hd = Quit) then acc else hd :: acc) [] lis)
  
  (* replace_quit (strip1 (change_to_prog (process_str "Push 2\nLocal x\nPush x\nPush 1\nAdd\nGlobal x\nPush x\nQuit")));; *)
  let deconstruct (g : global) =
    List.rev(List.fold_left(fun acc hd -> 
      List.append (match hd with
      | (x, y) -> y :: x :: []
      ) acc) [] g)
  
  (* deconstruct [(Name("x"), Int(1)); (Name("y"), Int(2))];; *)
  
  let separate_odds (lis : const list) =
    let x, y = List.fold_left(fun (acc, count) hd -> if(count mod 2 <> 0) then (hd :: acc, count + 1) else (acc, count + 1)) ([], 1) lis
    in List.rev x
  (* separate_odds [Name "x"; Int 1; Name "y"; Int 2];; *)
  
  let separate_evens (lis : const list) =
    let x, y = List.fold_left(fun (acc, count) hd -> if(count mod 2 == 0) then (hd :: acc, count + 1) else (acc, count + 1)) ([], 1) lis
    in List.rev x
  (* separate_evens [Name "x"; Int 1; Name "y"; Int 2];; *)

let get_helper (a : const) (tuple : const) =
  match tuple with
  | Tup(x) -> (let x, y = List.fold_left (fun (acc, count) hd -> 
      if(count == match a with | Int(x) -> x | _ -> 100) then
        (hd :: acc, count + 1)
      else
        (acc, count + 1)
    ) ([], 0) x
    in match x with
    |[y] -> y
    | _ -> Str("Error"))
  | _ -> Str("Error")

(* get_helper (Int(-1)) (Tup [Int 9; Int 4; Int 5]);; *)

let rec funclist_to_local (lis : func list) (env : local)=
  match lis with
  | [] -> []
  | (a, b, c) :: tl -> (a, Clo(a, b, c, env)) :: funclist_to_local tl env

(* funclist_to_local [(Name "name", Name "arg", [Push (Int 5); Quit]); (Name "name2", Name "arg2", [Push (Int 6); Quit])] [(Name "x", Int 5)];; *)

let obtain_top_local (l : local) =
  match l with
  | [] -> (Str("Error"), Str("Error"))
  | x :: tl -> x

let pop_local (l : local) (stop : int) =
  let x, y = List.fold_left (fun (acc, count) hd ->
    if(count < stop) then
      (acc, count + 1)
    else
      (hd :: acc, count + 1)
    ) ([], 0) l
  in List.rev x

let local_counter (lis : cmd list) =
  List.fold_left(fun count hd ->
    match hd with
    | Local(x) -> count + 1
    | _ -> count
    ) 0 lis

let pop_local2 (l : local) (lis : cmd list) = 
  match obtain_top_local l with
  | (a, b) ->
    match b with
    | Clo(a,b,c,e) -> l
    | _ -> pop_local l ((local_counter lis) + 1)

(* pop_local [(Name "f1", Name "x"); (Name "f2", Name "y"); (Name "f3", Name "z")] 1;; *)

(* let find_return (lis : cmd list) =
  List.fold_left(fun ret hd ->
    match hd with
    | Return -> true
    | _ -> ret
    ) false lis

let return (lis : func list) =
  List.fold_left (fun ret hd ->
    match hd with
    | (a, b, c) -> if(find_return c == false) then
        false
      else
        ret
  ) true lis *)

(* return [(Name "isOdd", Name "x", [Push (Int 5); Return; Quit]);
    (Name "isEven", Name "y", [Push (Int 7); Return; Quit]);
    (Name "isSome", Name "w", [Push (Int 8); Return; Quit])];; *)

let obtain_second_top (lis : const list) =
  match lis with
  | x :: y :: tl -> y
  | _ -> Str("Error")

  let rec eval (lis : cmd list) (stack : const list) (l : local) (g : global) =
    match stack with
    | Str("Error") :: t -> None
    | _ -> (
      match lis with
    | [] -> Some []
    | Push(a) :: t -> eval t (match a with
      | Name(x) -> push(local_finder(Name(x)) l g)(stack)
      | x -> push(x)(stack)
    ) l g
    | Pop :: t -> eval t (pop(stack)) l g
    | Add :: t -> eval t (add(stack)) l g
    | Sub :: t -> eval t (sub(stack)) l g
    | Mul :: t -> eval t (mul(stack)) l g
    | Div :: t -> eval t (div(stack)) l g
    | Swap :: t -> eval t (swap(stack)) l g
    | Neg :: t -> eval t (neg(stack)) l g
    | Concat :: t -> eval t (concat(stack)) l g
    | And :: t -> eval t (andCmd(stack)) l g
    | Or :: t -> eval t (orCmd(stack)) l g
    | Not :: t -> eval t (not(stack)) l g
    | Equal :: t -> eval t (equal(stack)) l g
    | Lte :: t -> eval t (lte(stack)) l g
    | Local(a) :: t -> eval t (pop(stack)) ((a, obtain_top(stack)) :: l) g
    | Global(a) :: t -> eval t (pop(stack)) l ((a, obtain_top(stack)) :: g)
    | BeginEnd(a) :: t -> eval t (get_head(strip2(eval a [] l g)) :: stack) l (List.combine (separate_odds(strip2(eval (replace_quit_g a) [] l g))) (separate_evens(strip2(eval (replace_quit_g a) [] l g))))
    | IfThenElse(a, b) :: t -> 
      if((eval a stack l g) = None || (eval b stack l g) = None) then
        None
      else
      (match obtain_top(stack) with
      | Int(1) -> (eval t (strip2(eval a (pop(stack)) l g))

      (List.combine (separate_odds(strip2(eval (replace_quit_l a) [] l g)))
      (separate_evens(strip2(eval (replace_quit_l a) [] l g))))
      (List.combine (separate_odds(strip2(eval (replace_quit_g a) [] l g)))
      (separate_evens(strip2(eval (replace_quit_g a) [] l g)))))

      | Int(0) -> (eval t (strip2(eval b (pop(stack)) l g))
      
      (List.combine (separate_odds(strip2(eval (replace_quit_l b) [] l g)))
      (separate_evens(strip2(eval (replace_quit_l b) [] l g))))
      (List.combine (separate_odds(strip2(eval (replace_quit_g b) [] l g)))
      (separate_evens(strip2(eval (replace_quit_g b) [] l g)))))

      | _ -> None)
    | InjL :: t -> eval t (injL(stack)) l g
    | InjR :: t -> eval t (injR(stack)) l g
    | Tuple(a) :: t -> 
      if((match a with | Int(x) -> x | _ -> 100) <= List.length stack) then
        eval t (Tup(tuple(a)(stack)) :: tuple_remain(a)(stack)) l g
      else
        None
    | Get(a) :: t -> 
      (match (obtain_top stack) with
      | Tup(x) ->
        (if((get_helper a (obtain_top stack)) <> Str("Error")) then 
          eval t (get_helper a (obtain_top stack) :: stack) l g
        else
          None)
      | _ -> None)
    | CaseLeftRight(a, b) :: t ->
      (match obtain_top(stack) with
      | Left(x) -> 
      if ((eval a (x :: pop(stack)) l g) = None) then
        None
      else 
        (eval t (strip2(eval a (x :: pop(stack)) l g))

      (List.combine (separate_odds(strip2(eval (replace_quit_l a) [] l g)))
      (separate_evens(strip2(eval (replace_quit_l a) [] l g))))
      (List.combine (separate_odds(strip2(eval (replace_quit_g a) [] l g)))
      (separate_evens(strip2(eval (replace_quit_g a) [] l g)))))

      | Right(x) ->
      if ((eval b (x :: pop(stack)) l g) = None) then
        None
      else 
        (eval t (strip2(eval b (x :: pop(stack)) l g))
      
      (List.combine (separate_odds(strip2(eval (replace_quit_l b) [] l g)))
      (separate_evens(strip2(eval (replace_quit_l b) [] l g))))
      (List.combine (separate_odds(strip2(eval (replace_quit_g b) [] l g)))
      (separate_evens(strip2(eval (replace_quit_g b) [] l g)))))
      
      | _ -> None)
    | Fun(a) :: t -> eval t stack (List.append (funclist_to_local a l) l) g
    | Call :: t -> 
      eval t ((match (strip2(eval
      (match (obtain_top stack) with | Clo(x,y,z,e) -> z | _ -> []) []
      (((match (obtain_top stack) with | Clo(x,y,z,e) -> y | _ -> Str("Error")), (obtain_second_top stack)) :: pop_local2 l (match (obtain_top stack) with | Clo(x,y,z,e) -> z | _ -> []))
      g)) with | [x] -> x | _ -> Str("Error")) :: (match stack with | x :: y :: tl -> tl | _ -> [Str("Error")]))
    l g
    | Return :: t -> eval t [(obtain_top stack)] l g
    | Quit :: t -> Some (stack)
    | Quit2 :: t -> Some (deconstruct g)
    | Quit3 :: t -> Some (deconstruct l)
    | _ -> None)

(* eval (strip1 (change_to_prog (process_str "Fun f1 x\nPush x\nReturn\nEnd\nFun f2 x\nPush 4\nReturn\n\nEnd\nQuit"))) [] [] [];; *)
(* eval (strip1 (change_to_prog (process_str "Push 3\nInjR\nCaseLeft\nPush 4\nAdd\nRight\nPush 5\nAdd\nEnd\nQuit"))) [] [] [];; *)
(* eval [Push (Name "x"); Quit] [] [(Name "my_arg", Int 3)] [];;*)

(* eval [Push (Int 5); Push (Int 1); Push (Str "\"two\""); Push (Int 3); Tuple (Int 3); Quit] [] [] [] [];; *)
(* eval [Push (Int 5); Push (Int 1); Push (Str "\"two\""); Push (Int 3); Quit] [] [] [];; *)

(* eval (strip1 (change_to_prog (process_str "Push 10\nInjR\nInjL\nQuit"))) [] [] [] [];; *)
(* eval (strip1 (change_to_prog (process_str "Fun f1 x\nPush x\nReturn\nMut f2 x\nPush x\nPush f1\nCall\nReturn\nEnd\nPush 3\nPush f2\nCall\nQuit"))) [] [] [];; *)

(* eval (strip1 (change_to_prog (process_str "Fun f1 x\nPush x\nReturn\nEnd\nPush 5\nPush f1\nCall\nQuit"))) [] [] [];; *)

(* eval (strip1 (change_to_prog (process_str "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 46\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nPush even\nCall\nReturn\nEnd\nMut even x\nPush 1\nPush x\nAdd\nLocal x\nPush x\nPush odd\nCall\nReturn\nEnd\nPush 5\nPush odd\nCall\nQuit"))) [] [] [];; *)
(* eval (strip1 (change_to_prog (process_str "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 10\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nPush even\nCall\nReturn\nEnd\nMut even x\nPush 1\nPush x\nAdd\nLocal x\nPush x\nPush odd\nCall\nReturn\nEnd\nPush 5\nPush odd\nCall\nQuit"))) [] [] [];; *)
(* eval (strip1 (change_to_prog (process_str "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 46\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nPush even\nCall\nReturn\nEnd\nMut even x\nPush 1\nPush x\nAdd\nLocal x\nPush x\nReturn\nEnd\nPush 5\nPush odd\nCall\nQuit"))) [] [] [];; *)
(* eval (strip1 (change_to_prog (process_str "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 46\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nReturn\nEnd\nEnd\nPush 5\nPush odd\nCall\nQuit"))) [] [] [];; *)
(* eval (strip1 (change_to_prog (process_str "Fun odd x\nPush x\nPush 2\nMul\nLocal x\nPush x\nPush 46\nEqual\nIfThen\nPush x\nReturn\nElse\nPush x\nReturn\nEnd\nEnd\nPush 5\nPush odd\nQuit"))) [] [] [];; *)

(* eval (strip1 (change_to_prog (process_str "Push 1\nFun func x\nReturn\nEnd\nPush func\nPush 5\nTuple 2\nQuit"))) [] [] [];; *)


let process (str : string) =
  (String.split_on_char '\n' str)

  let rec process_list2 (lis : const list) =
    String.concat "\n" (List.rev
    (List.fold_left(fun acc hd -> 
      (match hd with
      | Int(x) -> string_of_int x
      | Str(x) -> x
      | Name(x) -> x
      | Left(x) -> "Left " ^ process_list2 ([x])
      | Right(x) -> "Right " ^ process_list2 ([x])
      | Tup(x) -> "(" ^ (String.concat ", " (process (process_list2 x))) ^ ")"
      | Clo(x, y, z, e) -> "Clo (" ^ process_list2 ([x]) ^ ", " ^ process_list2([y]) ^ ")"
      | Empty -> ""
      ) :: acc) 
    [] lis))
  
  (* process_list2 [Clo (Name "foo", Name "my_arg", [Quit])];; *)
  (* process_list2 (strip2 (eval (strip1 (change_to_prog (process_str "Push \"hello\"\nInjL\nInjR\nQuit"))) [] [] [] []));; *)
  (* eval (strip1 (change_to_prog (process_str "Push 9\nLocal x\nPush x\nPush 4\nTuple 2\nQuit"))) [] [] [] [];; *)
  (* eval (strip1 (change_to_prog (process_str "Push 9\nLocal x\nPush x\nPush 4\nTuple 2\nQuit"))) [] [] [] [];; *)
  let write_file_example (file_path : string) (str : string): unit =
    let string_print = str in
    let fp = open_out file_path in
    let () = Printf.fprintf fp "%s" (string_print) in
      close_out fp
  
let hasQuit (lis : cmd list) =
  List.fold_left(fun quit hd ->
    match hd with
    | Quit -> true
    | _ -> quit
    ) false lis

  let interpreter (src : string) (output_file_path: string): unit =
    match eval (strip1 (change_to_prog (process_str src))) [] [] [] with 
    | Some x -> write_file_example (output_file_path) (process_list2 x)
    | None -> match (eval (strip1 (change_to_prog (process_str (src ^ "\nQuit")))) [] [] []) with
      | Some(x) -> write_file_example (output_file_path) ("")
      | _ -> write_file_example (output_file_path) ("\"Error\"")
  
  (* process_list2 (strip2 (eval (change_to_prog (process_str "Push 100\nPush 500\nDiv\nPush \"Python\"\nQuit")) [] [] []));; *)
  (* process_list2 (strip2 (eval (change_to_prog (process_str "Push 2\nLocal x\nPush x\nPush 1\nAdd\nGlobal x\nPush x\nQuit")) [] [] []));; *)
  
  (* interpreter "Push 40\nPush 60\nAdd\nPush 250\nDiv\nPush -2245\nDiv\nPush 529\nPush -529\nAdd\nPush 789\nDiv\nMul\nDiv\nQuit" "e.txt";; *)
  (* interpreter "Push 1\nLocal one\nBegin\nPush 2\nLocal two\nBegin\nPush one\nPush two\nSub\nEnd\nEnd\nIfThen\nPush \"True\"\nElse\nPush \"False\"\nEnd\nQuit" "f.txt";; *)