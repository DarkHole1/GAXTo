type currentStack = CalcStack | VarStack [@@deriving show]

(* let ( +: ) = Int64.add *)
(* let ( -: ) = Int64.sub *)
let ( *: ) = Int64.mul
let ( /: ) = Int64.div

type t = {
  calc_stack : int64 list;
  var_stack : int64 list;
  current : currentStack;
  variables : int64 list;
}
[@@deriving show]

type next = State of t

let create (_ : unit) =
  {
    calc_stack = [];
    var_stack = [];
    current = CalcStack;
    variables = Fun.const 0L |> List.init 26;
  }

let push_to_calc state x = { state with calc_stack = x :: state.calc_stack }
let push_to_var state x = { state with var_stack = x :: state.var_stack }

let parse_constant = function
  | '0' -> 0L
  | '1' -> 1L
  | '2' -> 2L
  | '3' -> 3L
  | '4' -> 4L
  | '5' -> 5L
  | '6' -> 6L
  | '7' -> 7L
  | '8' -> 8L
  | '9' -> 9L
  | 'A' -> 10L
  | 'B' -> 20L
  | 'C' -> 30L
  | 'D' -> 40L
  | 'E' -> 50L
  | 'F' -> 60L
  | 'G' -> 70L
  | 'H' -> 80L
  | 'I' -> 90L
  | 'J' -> 100L
  | 'K' -> 200L
  | 'L' -> 300L
  | 'M' -> 400L
  | 'N' -> 500L
  | 'O' -> 600L
  | 'P' -> 700L
  | 'Q' -> 800L
  | 'R' -> 900L
  | 'S' -> 1000L
  | 'T' -> 2000L
  | 'U' -> 3000L
  | 'V' -> 4000L
  | 'W' -> 5000L
  | 'X' -> 6000L
  | 'y' -> 7000L
  | 'Z' -> 8000L
  | _ -> invalid_arg "Unknown constant"

let parse_variable v = Char.code v |> Int64.of_int

let pop_current state =
  match state with
  | { current = CalcStack; calc_stack = x :: xs; _ } ->
      (x, { state with calc_stack = xs })
  | { current = VarStack; var_stack = x :: xs; _ } ->
      (x, { state with var_stack = xs })
  | _ -> invalid_arg "Empty current stack"

let get_variable state name =
  Char.code 'a' |> Int64.of_int |> Int64.sub name |> Int64.to_int
  |> List.nth state.variables

let pop_current_value state =
  let v, state' = pop_current state in
  match state.current with
  | VarStack -> (get_variable state v, state')
  | _ -> (v, state')

let get_alpha_beta state =
  let beta, state' = pop_current_value state in
  let alpha, state'' = pop_current_value state' in
  (alpha, beta, state'')

let push_current state x =
  match state.current with
  | CalcStack -> push_to_calc state x
  | VarStack -> push_to_var state x

let concat a b =
  if a = 0L then b
  else if b = 0L then a
  else
    let sign x = match x with 0L -> 1L | n -> n /: Int64.abs n in
    let s = sign a *: sign b in
    let a' = Int64.abs a |> Int64.to_string in
    let b' = Int64.abs b |> Int64.to_string in
    let c = Int64.of_string (a' ^ b') in
    s *: c

let run_math state chr =
  let op =
    match chr with
    | '+' -> Int64.add
    | '-' -> Int64.sub
    | '/' -> Int64.div
    | '*' -> Int64.mul
    | '_' -> concat
    | _ -> invalid_arg "Unknown math operationg"
  in
  let alpha, beta, state' = get_alpha_beta state in
  let rho = op alpha beta in
  push_current state' rho

let switch_stack state =
  match state.current with
  | CalcStack -> { state with current = VarStack }
  | VarStack -> { state with current = CalcStack }

let run_logical state chr =
  let to_bool x = Int64.compare x 0L <> 0 in
  let from_bool x = if x then 1L else 0L in
  let op =
    match chr with
    | '`' -> fun a b -> not (to_bool a || to_bool b)
    | '<' -> ( < )
    | '=' -> ( = )
    | '>' -> ( > )
    | _ -> invalid_arg "Unknown logical operator"
  in
  let alpha, beta, state' = get_alpha_beta state in
  let rho = op alpha beta |> from_bool in
  push_current state' rho

let peek state =
  match state with
  | { current = CalcStack; calc_stack = x :: _; _ } -> x
  | { current = VarStack; var_stack = x :: _; _ } -> get_variable state x
  | _ -> invalid_arg "Current stack is empty"

let run_print state chr =
  let beta = peek state in
  let () =
    match chr with
    | '?' -> Int64.to_string beta |> print_string
    | '$' when beta >= 32L && beta <= 126L ->
        Int64.to_int beta |> Char.chr |> print_char
    | '$' -> invalid_arg "Value isn't printable"
    | _ -> invalid_arg "Unknown print operator"
  in
  state

let set_variable state x =
  match state.var_stack with
  | y :: _ ->
      let y' = Char.code 'a' |> Int64.of_int |> Int64.sub y in
      if y' < 0L || y' >= (List.length state.variables |> Int64.of_int) then
        invalid_arg "No such variable"
      else
        {
          state with
          variables =
            List.mapi
              (fun i x' -> if Int64.of_int i = y' then x else x')
              state.variables;
        }
  | _ -> invalid_arg "Empty variable stack"

let run_stack state chr =
  match chr with
  | ':' -> (
      let alpha, state' = pop_current state in
      match state.current with
      | CalcStack -> set_variable state' alpha
      | VarStack ->
          {
            state' with
            calc_stack = get_variable state' alpha :: state'.calc_stack;
          })
  | ';' -> (
      match state.current with
      | CalcStack -> { state with calc_stack = List.rev state.calc_stack }
      | VarStack -> { state with var_stack = List.rev state.var_stack })
  | '~' ->
      let _, state' = pop_current state in
      state'
  | '%' -> (
      match state.current with
      | CalcStack -> { state with calc_stack = [] }
      | VarStack -> { state with var_stack = [] })
  | _ -> invalid_arg "Unknown stack operation"

let run_char state chr _rest =
  match chr with
  | '[' -> State state
  | ']' -> State state
  | '!' -> raise Exit
  | _ ->
      let just =
        match chr with
        | '0' .. '9' -> parse_constant chr |> push_to_calc state
        | 'A' .. 'Z' -> parse_constant chr |> push_to_calc state
        | 'a' .. 'z' -> parse_variable chr |> push_to_var state
        | '+' | '-' | '/' | '*' | '_' -> run_math state chr
        | '`' | '<' | '=' | '>' -> run_logical state chr
        | '?' | '$' -> run_print state chr
        | ':' | ';' | '~' | '%' -> run_stack state chr
        | '#' -> switch_stack state
        | _ -> state
      in
      State just

let run state code =
  let rec run_inner code state =
    match Seq.uncons code with
    | None -> state
    | Some (c, rest) -> (
        let res = run_char state c rest in
        match res with State s -> run_inner rest s)
  in
  let code = String.to_seq code in
  try run_inner code state with Exit -> state

let run' code =
  let i = create () in
  run i code

let%test_module _ =
  (module struct
    let i = run' "AZ09az"

    let%test "Basic Constants" =
      i.calc_stack = [ 9L; 0L; 8000L; 10L ] && i.var_stack = [ 122L; 97L ]

    (* ((40 - 30) / 10 + 9) * 5 = 50 *)
    let i = run' "DC-A/9+5*"
    let%test "Basic Math Operations" = i.calc_stack = [ 50L ]

    (*
          |    10 |   0 |   -10
      ----|-------|-----|------
       20 |  1020 |  20 | -1020
        0 |    10 |   0 |   -10
      -20 | -1020 | -20 |  1020
    *)
    let s =
      [
        [ "AB_"; "0B_"; "0A-B_" ];
        [ "A0_"; "00_"; "0A-0_" ];
        [ "A0B-_"; "00B-_"; "0A-0B-_" ];
      ]

    let s' = List.map (String.concat " ") s |> String.concat "\n"

    let r =
      [ [ 1020L; 20L; -1020L ]; [ 10L; 0L; -10L ]; [ -1020L; -20L; 1020L ] ]

    let r' = List.flatten r |> List.rev
    let i = run' s'
    let%test "Concat operator" = i.calc_stack = r'

    (* 10 > 20 => 0; 10 < 20 => 1; 10 = 20 => 0 *)
    let i = run' "AB>AB<AB="
    let%test "Test logical" = i.calc_stack = [ 0L; 1L; 0L ]
    let i = run' "00`01`10`11`"
    let%test "Test nor" = i.calc_stack = [ 0L; 0L; 0L; 1L ]

    (* a = 10 *)
    let i = run' "Aa:"

    let%test "Test assign to varibale" =
      i.var_stack = [ Char.code 'a' |> Int64.of_int ]
      && i.calc_stack = []
      && i.variables = 10L :: List.init 25 (Fun.const 0L)

    let i = run' "Aa:#:"

    let%test "Test variable to stack" =
      i.var_stack = [] && i.calc_stack = [ 10L ]
      && i.variables = 10L :: List.init 25 (Fun.const 0L)
  end)
