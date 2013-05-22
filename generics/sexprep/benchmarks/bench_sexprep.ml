open Core.Std
open Core_extended
open Typereplib.Std

let int_list_command = Command.basic
    ~summary:"sexprep benchmarks"
    Command.Spec.( empty
      +> flag "-size" (optional_with_default 2000 int)
         ~doc:"Size of list to deserialize (default 2000)"
      +> flag "-serial" no_arg
         ~doc:"direction of test: serialization or deserialization (default deserial)"
    )
    (fun size is_serial () ->
      let module M = struct
        type 'a t = 'a list list with typerep, sexp
      end in
      let list_of_int = List.init size ~f:ident in
      let values = List.init ~f:(fun _ -> list_of_int) size in
      let `generic x_of_sexp = Sexprep.t_of_sexp (M.typerep_of_t typerep_of_int) in
      let `generic sexp_of_x = Sexprep.sexp_of_t (M.typerep_of_t typerep_of_int) in
      let untyped_values =
        let `generic untyped_of_t = Tagged.Of_typed.of_typerep (M.typerep_of_t typerep_of_int) in
        untyped_of_t values
      in
      let `generic un_of_sexp =
        Sexprep.Tagged.Of_sexp.of_typestruct
          (Type_struct.of_typerep (M.typerep_of_t typerep_of_int))
      in
      let `generic sexp_of_un =
        Sexprep.Tagged.Sexp_of.of_typestruct
          (Type_struct.of_typerep (M.typerep_of_t typerep_of_int))
      in
      let sexp = M.sexp_of_t sexp_of_int values in
      let tests =
        if is_serial then
          [ Bench.Test.create ~name:"sexprep" (fun () ->
            let _sexp = sexp_of_x values in ())
          ; Bench.Test.create ~name:"untyped" (fun () ->
            let _sexp = sexp_of_un untyped_values in ())
          ; Bench.Test.create ~name:"withsexp" (fun () ->
            let _sexp = M.sexp_of_t sexp_of_int values in ())]
        else
          [ Bench.Test.create ~name:"sexprep" (fun () ->
            let _values = x_of_sexp sexp in ())
          ; Bench.Test.create ~name:"untyped" (fun () ->
            let _values = un_of_sexp sexp in ())
          ; Bench.Test.create ~name:"withsexp" (fun () ->
            let _values = M.t_of_sexp int_of_sexp sexp in ())]
      in
      Bench.bench ~verbosity:`Mid tests
    )

let record_list_command = Command.basic
    ~summary:"sexprep benchmarks"
    Command.Spec.( empty
      +> flag "-size" (optional_with_default 2000 int)
         ~doc:"Size of list to deserialize (default 2000)"
      +> flag "-reorder" no_arg
         ~doc:"Whether to reorder the sexps before deserialization"
      +> flag "-serial" no_arg
         ~doc:"direction of test: serialization or deserialization (default deserial)"
    )
    (fun size reorder is_serial () ->
      let module M = struct
        type 'a r = {
          one:'a;
          two:int;
          three:'a;
          four:int;
          five:'a;
          six:int;
          seven:'a;
          eight:int;
          nine:'a;
          ten:int;
          eleven:'a;
          twelve:int;
          thirteen:'a;
          fourteen:int;
          fiveteen:'a;
          sixteen:int;
          seventeen:'a;
          eighteen:int;
          nineteen:'a;
          twenty:int;
          } with typerep, sexp
        type 'a t = 'a r list list with typerep, sexp
      end in
      let list_of_record = List.init size ~f:(fun n -> {
          M.one="hello";
          two=n;
          three="hello";
          four=n;
          five="hello";
          six=n;
          seven="hello";
          eight=n;
          nine="hello";
          ten=n;
          eleven="hello";
          twelve=n;
          thirteen="hello";
          fourteen=n;
          fiveteen="hello";
          sixteen=n;
          seventeen="hello";
          eighteen=n;
          nineteen="hello";
          twenty=n;
        }) in
      let values = List.init ~f:(fun _ -> list_of_record) size in
      let initial_sexp = M.sexp_of_t (sexp_of_string) values in
      let sexp =
        if reorder
        then match initial_sexp with
        | Sexp.List list_lists_of_recs -> Sexp.List
          (List.map list_lists_of_recs ~f:(function
            | Sexp.List list_recs -> Sexp.List (List.permute list_recs)
            | _ -> raise (Failure "failed before tests: unexpected sexp structure")))
        | _ -> raise (Failure "failed before tests: unexpected sexp structure")
        else initial_sexp
      in
      let `generic sexp_of_x = Sexprep.sexp_of_t (M.typerep_of_t typerep_of_string) in
      let `generic x_of_sexp = Sexprep.t_of_sexp (M.typerep_of_t typerep_of_string) in
      let untyped_values =
        let `generic untyped_of_t =
          Tagged.Of_typed.of_typerep (M.typerep_of_t typerep_of_string)
        in
        untyped_of_t values
      in
      let `generic un_of_sexp =
        Sexprep.Tagged.Of_sexp.of_typestruct
          (Type_struct.of_typerep (M.typerep_of_t typerep_of_string))
      in
      let `generic sexp_of_un =
        Sexprep.Tagged.Sexp_of.of_typestruct
          (Type_struct.of_typerep (M.typerep_of_t typerep_of_string))
      in
      let tests =
        if is_serial then
          [ Bench.Test.create ~name:"sexprep" (fun () ->
            let _sexp = sexp_of_x values in ())
          ; Bench.Test.create ~name:"untyped" (fun () ->
            let _sexp = sexp_of_un untyped_values in ())
          ; Bench.Test.create ~name:"withsexp" (fun () ->
            let _sexp = M.sexp_of_t sexp_of_string values in ())]
        else
          [ Bench.Test.create ~name:"sexprep" (fun () ->
            let _values = x_of_sexp sexp in ())
          ; Bench.Test.create ~name:"untyped" (fun () ->
            let _values = un_of_sexp sexp in ())
          ; Bench.Test.create ~name:"withsexp" (fun () ->
            let _values = M.t_of_sexp string_of_sexp sexp in ())]
      in
      Bench.bench ~verbosity:`Mid tests
    )

let variant_list_command =
  Command.basic
    ~summary:"sexprep benchmarks"
    Command.Spec.( empty
      +> flag "-size" (optional_with_default 2000 int)
         ~doc:"Size of list to (de)serialize (default 2000)"
      +> flag "-serial" no_arg
         ~doc:"direction of test: serialization or deserialization (default deserial)"
    )
  (fun size is_serial () ->
    let module M = struct
      type v =
      | Foo
      | Bar of int
      | Baz of int * int
      with typerep, sexp
      type t = v list list with typerep, sexp
    end in
    let values = List.init size
      ~f:(fun _ -> List.init size
        ~f:(fun n ->
            if Int.equal n 0 then M.Foo
            else if Int.equal (n % 2) 0
            then M.Bar n
            else M.Baz (n,n-1)))
    in
    let `generic sexp_of_x = Sexprep.sexp_of_t M.typerep_of_t in
    let `generic x_of_sexp = Sexprep.t_of_sexp M.typerep_of_t in
    let untyped_values =
        let `generic untyped_of_t = Tagged.Of_typed.of_typerep M.typerep_of_t in
        untyped_of_t values
    in
    let `generic un_of_sexp =
      Sexprep.Tagged.Of_sexp.of_typestruct (Type_struct.of_typerep M.typerep_of_t)
    in
    let `generic sexp_of_un =
      Sexprep.Tagged.Sexp_of.of_typestruct (Type_struct.of_typerep M.typerep_of_t)
    in
    let sexp = M.sexp_of_t values in
    let tests =
      if is_serial then
        [ Bench.Test.create ~name:"sexprep" (fun () ->
          let _sexp = sexp_of_x values in ())
        ; Bench.Test.create ~name:"untyped" (fun () ->
          let _sexp = sexp_of_un untyped_values in ())
        ; Bench.Test.create ~name:"withsexp" (fun () ->
          let _sexp = M.sexp_of_t values in ())
        ]
      else
        [ Bench.Test.create ~name:"sexprep" (fun () ->
          let _values = x_of_sexp sexp in ())
        ; Bench.Test.create ~name:"untyped" (fun () ->
          let _values = un_of_sexp sexp in ())
        ; Bench.Test.create ~name:"withsexp" (fun () ->
          let _values = M.t_of_sexp sexp in ())]
    in
    Bench.bench ~verbosity:`Mid tests
    )


let tree_command = Command.basic
    ~summary:"sexprep benchmarks"
    Command.Spec.( empty
      +> flag "-depth" (optional_with_default 20 int)
         ~doc:"Depth of tree to serialize (default 20)"
      +> flag "-serial" no_arg
         ~doc:"direction of test: serialization or deserialization (default deserial)"
    )
    (fun size is_serial () ->
      let module M = struct
        type t =
          | Leaf
          | Green_Node of t * t
          | Red_Node of t * t
          | Black_Node of t * t
          | Blue_Node of t * t
          | Node2 of t * t
          | Node_2 of t * t
          | Binary_Node of t * t
          | Foo
          | Bar
          | Baz
          | Bax
        with typerep, sexp
      end in
      let rec producer n =
        if n > 0
        then M.Node2 (producer (n-1), producer (n-1))
        else M.Leaf
      in
      let values = producer size in
      let sexp = M.sexp_of_t values in
      let `generic x_of_sexp = Sexprep.t_of_sexp M.typerep_of_t in
      let `generic sexp_of_x = Sexprep.sexp_of_t M.typerep_of_t in
      let untyped_values =
        let `generic untyped_of_t = Tagged.Of_typed.of_typerep M.typerep_of_t in
        untyped_of_t values
      in
      let `generic un_of_sexp =
        Sexprep.Tagged.Of_sexp.of_typestruct (Type_struct.of_typerep M.typerep_of_t)
      in
      let `generic sexp_of_un =
        Sexprep.Tagged.Sexp_of.of_typestruct (Type_struct.of_typerep M.typerep_of_t)
      in
      let tests =
        if is_serial then
          [ Bench.Test.create ~name:"sexprep" (fun () ->
            let _sexp = sexp_of_x values in ())
          ; Bench.Test.create ~name:"untyped" (fun () ->
            let _sexp = sexp_of_un untyped_values in ())
          ; Bench.Test.create ~name:"withsexp" (fun () ->
            let _sexp = M.sexp_of_t values in ())]
        else
          [ Bench.Test.create ~name:"sexprep" (fun () ->
            let _values = x_of_sexp sexp in ())
          ; Bench.Test.create ~name:"untyped" (fun () ->
            let _values = un_of_sexp sexp in ())
          ; Bench.Test.create ~name:"withsexp" (fun () ->
            let _values = M.t_of_sexp sexp in ())]
      in
      Bench.bench ~verbosity:`Mid tests
    )

let allocate_small_maps = Command.basic
    ~summary:"sexprep benchmarks"
    Command.Spec.( empty
      +> flag "-size" (optional_with_default 1000 int)
         ~doc:"Number of times to repeat test (default 1000)"
      +> flag "-mapsize" (optional_with_default 10 int)
         ~doc:"Size of map to build (default 10)"
    )
    (fun size small_map_size () ->
      let values = List.init small_map_size ~f:(fun n -> Int.to_string n, n) in
      let tries = List.init size ~f:ident in
      Bench.bench ~verbosity:`Mid
        [ Bench.Test.create ~name:"flat_map" (fun () ->
          let _map = List.map tries ~f:(fun _ -> Flat_map.Flat_string_map.of_alist values)
          in ())
        ; Bench.Test.create ~name:"string map" (fun () ->
          let _value = List.map tries  ~f:(fun _ -> String.Map.of_alist values)
          in ());
      ]
    )

let look_up_maps = Command.basic
    ~summary:"sexprep benchmarks"
    Command.Spec.( empty
      +> flag "-size" (optional_with_default 1000 int)
         ~doc:"Number of times to repeat test (default 1000)"
      +> flag "-mapsize" (optional_with_default 10 int)
         ~doc:"Size of map to build (default 10)"
    )
    (fun size small_map_size () ->
      let values = List.init small_map_size ~f:(fun n -> Int.to_string n, n) in
      let flat_map = Flat_map.Flat_string_map.of_alist values in
      let string_map = String.Map.of_alist_exn values in
      let goals =
        List.init size
        ~f:(fun _ -> List.map values ~f:(fun (key,_value) -> key))
      in
      Bench.bench ~verbosity:`Mid
        [ Bench.Test.create ~name:"flat_map opt_to_exn" (fun () ->
          let _values = List.map goals
            ~f:(fun goal_list -> List.map goal_list ~f:(fun goal ->
              match Flat_map.Flat_string_map.find flat_map goal with
              | None -> raise (Failure "invalid sexp")
              | Some x -> x))
          in ())
        ; Bench.Test.create ~name:"string_map" (fun () ->
          let _valued = List.map goals
            ~f:(fun goal_list -> List.map goal_list
              ~f:(fun goal -> String.Map.find string_map goal))
          in ());
      ]
    )

let command = Command.group ~summary:"Benchmarks" [
  "int-list", int_list_command;
  "record-list", record_list_command;
  "variant-list", variant_list_command;
  "tree", tree_command;
  "look-ups", look_up_maps;
  "small-maps", allocate_small_maps;
]

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
