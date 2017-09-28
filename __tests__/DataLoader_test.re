open Jest;

describe
  "Primary API"
  (
    fun () => {
      open Expect;
      open! Expect.Operators;
      testPromise
        "it builds a really simple data loader"
        (
          fun () => {
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => Js.Promise.resolve strings;
              let options: DataLoader.options = {batch: false, maxBatchSize: 256, cache: true};
            };
            module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
            IdentityLoader.load "1"
            |> Js.Promise.then_ (
                 fun loadedVal => Js.Promise.resolve (expect loadedVal |> toBe "1")
               )
          }
        );
      testPromise
        "it supports loading multiple keys in one call"
        (
          fun () => {
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => Js.Promise.resolve strings;
              let options: DataLoader.options = {batch: true, maxBatchSize: 256, cache: true};
            };
            module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
            IdentityLoader.loadMany [|"1", "2"|]
            |> Js.Promise.then_ (
                 fun loadedVal => Js.Promise.resolve (expect loadedVal |> toEqual [|"1", "2"|])
               )
          }
        );
      testPromise
        "it batches multiple requests"
        (
          fun () => {
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => Js.Promise.resolve strings;
              let options: DataLoader.options = {batch: false, maxBatchSize: 256, cache: true};
            };
            module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
            Js.Promise.all [|IdentityLoader.load "1", IdentityLoader.load "2"|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2"|])
               )
          }
        );
      testPromise
        "it batches multiple requests with max batch sizes"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                let _ = Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
            let load1 = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            let load3 = IdentityLoader.load "3";
            Js.Promise.all [|load1, load2, load3|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls)
                     |> toEqual ([|"1", "2", "3"|], [|[|"1", "2"|], [|"3"|]|])
                   )
               )
          }
        );
      testPromise
        "coalesces identical requests"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                let _ = Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
            let promise1a = IdentityLoader.load "1";
            let promise1b = IdentityLoader.load "1";
            Js.Promise.all [|promise1a, promise1b|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls) |> toEqual ([|"1", "1"|], [|[|"1"|]|])
                   )
               )
          }
        )
    }
  );

describe
  "caches repeated requests"
  (
    fun () => {
      open Expect;
      open! Expect.Operators;
      let calls = [||];
      module IdentityLoaderImpl = {
        type value = string;
        let batchLoadFun strings => {
          let _ = Js.Array.push strings calls;
          Js.Promise.resolve strings
        };
        let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
      };
      module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
      testPromise
        "initial call"
        (
          fun () => {
            let load1a = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls) |> toEqual ([|"1", "2"|], [|[|"1", "2"|]|])
                   )
               )
          }
        );
      testPromise
        "repeted requests"
        (
          fun () => {
            let load1b = IdentityLoader.load "1";
            let load3 = IdentityLoader.load "3";
            Js.Promise.all [|load1b, load3|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls)
                     |> toEqual ([|"1", "3"|], [|[|"1", "2"|], [|"3"|]|])
                   )
               )
          }
        )
    }
  );

describe
  "clear single value"
  (
    fun () => {
      open Expect;
      open! Expect.Operators;
      let calls = [||];
      module IdentityLoaderImpl = {
        type value = string;
        let batchLoadFun strings => {
          let _ = Js.Array.push strings calls;
          Js.Promise.resolve strings
        };
        let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
      };
      module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
      testPromise
        "can get a value"
        (
          fun () => {
            let load1a = IdentityLoader.load "1";
            let load2a = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2a|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls) |> toEqual ([|"1", "2"|], [|[|"1", "2"|]|])
                   )
               )
          }
        );
      testPromise
        "clears single value in loader"
        (
          fun () => {
            IdentityLoader.clear "1";
            let load1b = IdentityLoader.load "1";
            let load2b = IdentityLoader.load "2";
            Js.Promise.all [|load1b, load2b|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls)
                     |> toEqual ([|"1", "2"|], [|[|"1", "2"|], [|"1"|]|])
                   )
               )
          }
        )
    }
  );

describe
  "clear all values"
  (
    fun () => {
      open Expect;
      open! Expect.Operators;
      let calls = [||];
      module IdentityLoaderImpl = {
        type value = string;
        let batchLoadFun strings => {
          let _ = Js.Array.push strings calls;
          Js.Promise.resolve strings
        };
        let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
      };
      module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
      testPromise
        "loads initial data"
        (
          fun () => {
            let load1a = IdentityLoader.load "1";
            let load2a = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2a|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls) |> toEqual ([|"1", "2"|], [|[|"1", "2"|]|])
                   )
               )
          }
        );
      testPromise
        "clears all values in loader"
        (
          fun () => {
            IdentityLoader.clearAll ();
            let load1b = IdentityLoader.load "1";
            let load2b = IdentityLoader.load "2";
            Js.Promise.all [|load1b, load2b|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls)
                     |> toEqual ([|"1", "2"|], [|[|"1", "2"|], [|"1", "2"|]|])
                   )
               )
          }
        )
    }
  );

describe
  "Priming the cache"
  (
    fun () => {
      open Expect;
      open! Expect.Operators;
      testPromise
        "it allows priming the cache"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                let _ = Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
            IdentityLoader.prime "1" "1";
            let load1 = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            Js.Promise.all [|load1, load2|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (
                     expect (loadedValues, calls) |> toEqual ([|"1", "2"|], [|[|"2"|]|])
                   )
               )
          }
        );
      testPromise
        "it does not prime keys that already exist"
        (
          fun () => {
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => Js.Promise.resolve strings;
              let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
            IdentityLoader.prime "1" "A";
            IdentityLoader.prime "1" "B";
            let load1 = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            Js.Promise.all [|load1, load2|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"A", "2"|])
               )
          }
        )
    }
  );

describe
  "Priming the cache by force"
  (
    fun () => {
      open Expect;
      open! Expect.Operators;
      module IdentityLoaderImpl = {
        type value = string;
        let batchLoadFun strings => Js.Promise.resolve strings;
        let options: DataLoader.options = {batch: true, maxBatchSize: 2, cache: true};
      };
      module IdentityLoader = DataLoader.Make IdentityLoaderImpl;
      testPromise
        "allows priming the cache"
        (
          fun () => {
            IdentityLoader.prime "1" "A";
            let load1a = IdentityLoader.load "1";
            let load2a = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2a|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"A", "2"|])
               )
          }
        );
      testPromise
        "allows forcefully priming the cache"
        (
          fun () => {
            IdentityLoader.clear "1";
            IdentityLoader.prime "1" "X";
            IdentityLoader.clear "2";
            IdentityLoader.prime "2" "Y";
            let load1b = IdentityLoader.load "1";
            let load2b = IdentityLoader.load "2";
            Js.Promise.all [|load1b, load2b|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"X", "Y"|])
               )
          }
        )
    }
  );
