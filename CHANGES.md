## 113.24.00

- Add whether record fields are mutable.

## 112.24.00

- Remove unused "bin_proj" rewriter.

## 112.17.00

- Split out typerep_extended which is now using core_kernel

## 111.06.00

- Renamed `Typerep` libraries for more consistency with the rest of
  the framework.

    ```ocaml
    Typerep_kernel --> Typerep_lib
    Typerep_core   --> Typerep_extended
    Typereplib     --> Typerep_experimental
    ```

