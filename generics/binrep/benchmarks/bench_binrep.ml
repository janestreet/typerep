open Core.Std
open Typerep_experimental.Std

module Bench = Core_extended.Deprecated_bench

open Bin_prot

module IntListList = struct
  type t = int list list with typerep, bin_io

  let generate_list size =
    let helper ~f = List.init size ~f in
    helper ~f:(fun _ -> helper ~f:ident)

  let get_bin_tools list_size =
    let values = generate_list list_size in
    let buf_size = bin_size_t values in
    let buf = Common.create_buf buf_size in
    (values, buf)
end

module Records = struct
  module Record = struct
    type 'a t = {foo:'a; bar:int} with typerep, bin_io

    let list_of_ts size =
      List.init size ~f:(fun i -> { foo = "hello" ; bar = i })
  end

  type 'a t = 'a Record.t list list with typerep, bin_io

  let generate_list size = List.init ~f:(fun _ -> Record.list_of_ts size) size

  let get_bin_tools list_size =
    let values = generate_list list_size in
    let buf_size = bin_size_t bin_size_string values in
    let buf = Common.create_buf buf_size in
    (values, buf)
end

module Tree = struct
  type t = Leaf | Node of t * t with typerep, bin_io

  let rec generate_tree depth =
    if depth > 0 then
      Node (generate_tree (depth-1), generate_tree (depth-1))
    else
      Leaf

  let get_bin_tools tree_depth =
    let values = generate_tree tree_depth in
    let buf_size = bin_size_t values in
    let buf = Common.create_buf buf_size in
    (values, buf)
end

let deserialize_int_list_command = Command.basic
    ~summary:"bin-rep benchmarks"
    Command.Spec.( empty
      +> flag "-size" (optional_with_default 2000 int)
         ~doc:"Size of list to deserialize (default 2000)"
    )
    (fun size () ->
      let t_read =
        let `generic reader = Binrep.bin_reader_t IntListList.typerep_of_t in
        reader.Type_class.read
      in
      let t_obj_read =
        let `generic reader =
          let typerep =
            Type_struct.recreate_dynamically_typerep_for_test IntListList.typerep_of_t
          in
          Binrep.bin_reader_t typerep
        in
        reader.Type_class.read
      in
      let u_read =
        let str = Type_struct.of_typerep IntListList.typerep_of_t in
        let `generic reader = Binrep.Tagged.bin_reader_t str in
        reader.Type_class.read
      in
      let (values,buf) = IntListList.get_bin_tools size in
      ignore ( IntListList.bin_write_t buf ~pos:0 values ) ;
      Bench.bench ~verbosity:`Mid [
        Bench.Test.create ~name:"with bin_io deserialize int list list" (fun () ->
          let pos_ref = ref 0 in
          let _value = IntListList.bin_read_t buf ~pos_ref
          in ());

        Bench.Test.create ~name:"deserialize int list list" (fun () ->
          let pos_ref = ref 0 in
          let _value = t_read buf ~pos_ref in
          ());

        Bench.Test.create ~name:"deserialize int list list (obj)" (fun () ->
          let pos_ref = ref 0 in
          let _value = t_obj_read buf ~pos_ref in
          ());

        Bench.Test.create ~name:"deserialize int list list (untyped)" (fun () ->
          let pos_ref = ref 0 in
          let _value = u_read buf ~pos_ref in
          ());
      ]
    )

let serialize_int_list_command = Command.basic
  ~summary:"bin-rep benchmarks"
  Command.Spec.( empty
                 +> flag "-size" (optional_with_default 2000 int)
                   ~doc:"Size of list to serialize (default 2000)"
  )
  (fun size () ->
    let t_write =
      let `generic writer = Binrep.bin_writer_t IntListList.typerep_of_t in
      writer.Type_class.write
    in
    let t_obj_write =
      let `generic writer =
        let typerep =
          Type_struct.recreate_dynamically_typerep_for_test IntListList.typerep_of_t
        in
        Binrep.bin_writer_t typerep
      in
      writer.Type_class.write
    in
    let u_write =
      let str = Type_struct.of_typerep IntListList.typerep_of_t in
      let `generic writer = Binrep.Tagged.bin_writer_t str in
      writer.Type_class.write
    in
    let (values,buf) = IntListList.get_bin_tools size in
    let u_values =
      let `generic of_typed = Tagged.Of_typed.of_typerep IntListList.typerep_of_t in
      of_typed values
    in
    Bench.bench ~verbosity:`Mid [
      Bench.Test.create ~name:"with bin_io serialize int list list" (fun () ->
        ignore ( IntListList.bin_write_t buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize int list list" (fun () ->
        ignore ( t_write buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize int list list (obj)" (fun () ->
        ignore ( t_obj_write buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize int list list (untyped)" (fun () ->
        ignore ( u_write buf ~pos:0 u_values )
      );
    ]
  )

let deserialize_record_list_command = Command.basic
  ~summary:"bin-rep benchmarks"
  Command.Spec.( empty
                 +> flag "-size" (optional_with_default 2000 int)
                   ~doc:"Size of list to deserialize (default 2000)"
  )
  (fun size () ->
    let typerep = Records.typerep_of_t typerep_of_string in
    let t_read =
      let `generic reader = Binrep.bin_reader_t typerep in
      reader.Type_class.read
    in
    let t_obj_read =
      let `generic reader =
        let typerep =
          Type_struct.recreate_dynamically_typerep_for_test typerep
        in
        Binrep.bin_reader_t typerep
      in
      reader.Type_class.read
    in
    let u_read =
      let str = Type_struct.of_typerep typerep in
      let `generic reader = Binrep.Tagged.bin_reader_t str in
      reader.Type_class.read
    in
    let (values, buf) = Records.get_bin_tools size in
    ignore ( Records.bin_write_t Write.bin_write_string buf ~pos:0 values ) ;
    Bench.bench ~verbosity:`Mid [
      Bench.Test.create ~name:"with bin_io deserialize record list list" (fun () ->
        let pos_ref = ref 0 in
        ignore ( Records.bin_read_t Read.bin_read_string buf ~pos_ref )
      ) ;

      Bench.Test.create ~name:"bin_read deserialize record list list" (fun () ->
        let pos_ref = ref 0 in
        ignore ( t_read buf ~pos_ref )
      ) ;

      Bench.Test.create ~name:"bin_read deserialize record list list (obj)" (fun () ->
        let pos_ref = ref 0 in
        ignore ( t_obj_read buf ~pos_ref )
      ) ;

      Bench.Test.create ~name:"bin_read deserialize record list list (untyped)" (fun () ->
        let pos_ref = ref 0 in
        ignore ( u_read buf ~pos_ref )
      ) ;
    ]
  )

let serialize_record_list_command = Command.basic
  ~summary:"sexprep benchmarks"
  Command.Spec.( empty
                 +> flag "-size" (optional_with_default 2000 int)
                   ~doc:"Size of list to serialize (default 2000)"
  )
  (fun size () ->
    let typerep = Records.typerep_of_t typerep_of_string in
    let t_write =
      let `generic writer = Binrep.bin_writer_t typerep in
      writer.Type_class.write
    in
    let t_obj_write =
      let `generic writer =
        let typerep =
          Type_struct.recreate_dynamically_typerep_for_test typerep
        in
        Binrep.bin_writer_t typerep
      in
      writer.Type_class.write
    in
    let u_write =
      let str = Type_struct.of_typerep typerep in
      let `generic writer = Binrep.Tagged.bin_writer_t str in
      writer.Type_class.write
    in
    let (values, buf) = Records.get_bin_tools size in
    let u_values =
      let `generic of_typed = Tagged.Of_typed.of_typerep typerep in
      of_typed values
    in
    Bench.bench ~verbosity:`Mid [
      Bench.Test.create ~name:"with bin_io serialize record list list" (fun () ->
        ignore ( Records.bin_write_t Write.bin_write_string buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize record list list" (fun () ->
        ignore ( t_write buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize record list list (obj)" (fun () ->
        ignore ( t_obj_write buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize record list list (untyped)" (fun () ->
        ignore ( u_write buf ~pos:0 u_values )
      );
    ]
  )


let serialize_tree_command = Command.basic
  ~summary:"sexprep benchmarks"
  Command.Spec.( empty
                 +> flag "-depth" (optional_with_default 20 int)
                   ~doc:"Depth of tree to serialize (default 20)"
  )
  (fun d () ->
    let t_write =
      let `generic writer = Binrep.bin_writer_t Tree.typerep_of_t in
      writer.Type_class.write
    in
    let t_obj_write =
      let `generic writer =
        let typerep =
          Type_struct.recreate_dynamically_typerep_for_test Tree.typerep_of_t
        in
        Binrep.bin_writer_t typerep
      in
      writer.Type_class.write
    in
    let u_write =
      let str = Type_struct.of_typerep Tree.typerep_of_t in
      let `generic writer = Binrep.Tagged.bin_writer_t str in
      writer.Type_class.write
    in
    let (values,buf) = Tree.get_bin_tools d in
    let u_values =
      let `generic of_typed = Tagged.Of_typed.of_typerep Tree.typerep_of_t in
      of_typed values
    in
    Bench.bench ~verbosity:`Mid [
      Bench.Test.create ~name:"with bin_io serialize tree" (fun () ->
        ignore ( Tree.bin_write_t buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize tree" (fun () ->
        ignore ( t_write buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize tree (obj)" (fun () ->
        ignore ( t_obj_write buf ~pos:0 values )
      );

      Bench.Test.create ~name:"serialize tree (untyped)" (fun () ->
        ignore ( u_write buf ~pos:0 u_values )
      );
    ]
  )

let deserialize_tree_command = Command.basic
  ~summary:"sexprep benchmarks"
  Command.Spec.( empty
                 +> flag "-depth" (optional_with_default 20 int)
                   ~doc:"Depth of tree to deserialize (default 20)"
  )
  (fun d () ->
    let t_read =
      let `generic reader = Binrep.bin_reader_t Tree.typerep_of_t in
      reader.Type_class.read
    in
    let t_obj_read =
      let `generic reader =
        let typerep =
          Type_struct.recreate_dynamically_typerep_for_test Tree.typerep_of_t
        in
        Binrep.bin_reader_t typerep
      in
      reader.Type_class.read
    in
    let u_read =
      let str = Type_struct.of_typerep Tree.typerep_of_t in
      let `generic reader = Binrep.Tagged.bin_reader_t str in
      reader.Type_class.read
    in
    let (values,buf) = Tree.get_bin_tools d in
    ignore ( Tree.bin_write_t buf ~pos:0 values ) ;
    Bench.bench ~verbosity:`Mid [

      Bench.Test.create ~name:"with bin_io deserialize tree" (fun () ->
        let pos_ref = ref 0 in
        ignore ( Tree.bin_read_t buf ~pos_ref )
      ) ;

      Bench.Test.create ~name:"bin_read deserialize tree" (fun () ->
        let pos_ref = ref 0 in
        ignore ( t_read buf ~pos_ref )
      ) ;

      Bench.Test.create ~name:"bin_read deserialize tree (obj)" (fun () ->
        let pos_ref = ref 0 in
        ignore ( t_obj_read buf ~pos_ref )
      ) ;

      Bench.Test.create ~name:"bin_read deserialize tree (untyped)" (fun () ->
        let pos_ref = ref 0 in
        ignore ( u_read buf ~pos_ref )
      ) ;

    ]
  )


let command = Command.group ~summary:"Benchmarks" [
  "int-list-serial", serialize_int_list_command;
  "int-list-deserial", deserialize_int_list_command;
  "record-list-serial", serialize_record_list_command;
  "record-list-deserial", deserialize_record_list_command;
  "tree-serial", serialize_tree_command;
  "tree-deserial", deserialize_tree_command
]

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
