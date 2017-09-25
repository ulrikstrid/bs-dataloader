open Jest;

open Dataloader;

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
              let options: options = {batch: false, maxBatchSize: 256, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
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
              let options: options = {batch: true, maxBatchSize: 256, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
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
              let options: options = {batch: false, maxBatchSize: 256, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
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
                Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            let load1 = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            let load3 = IdentityLoader.load "3";
            Js.Promise.all [|load1, load2, load3|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1", "2"|], [|"3"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2", "3"|])
                 }
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
                Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            let promise1a = IdentityLoader.load "1";
            let promise1b = IdentityLoader.load "1";
            Js.Promise.all [|promise1a, promise1b|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "1"|])
                 }
               )
          }
        );
      testPromise
        "caches repeated requests"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            let load1a = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1", "2"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2"|])
                 }
               );
            let load1b = IdentityLoader.load "1";
            let load3 = IdentityLoader.load "3";
            Js.Promise.all [|load1b, load3|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1", "2"|], [|"3"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "3"|])
                 }
               )
          }
        );
      testPromise
        "clears single value in loader"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            let load1a = IdentityLoader.load "1";
            let load2a = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2a|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1", "2"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2"|])
                 }
               );
            IdentityLoader.clear "1";
            let load1b = IdentityLoader.load "1";
            let load2b = IdentityLoader.load "2";
            Js.Promise.all [|load1b, load2b|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1", "2"|], [|"1"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2"|])
                 }
               )
          }
        );
      testPromise
        "clears all values in loader"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            let load1a = IdentityLoader.load "1";
            let load2a = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2a|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1", "2"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2"|])
                 }
               );
            IdentityLoader.clearAll ();
            let load1b = IdentityLoader.load "1";
            let load2b = IdentityLoader.load "2";
            Js.Promise.all [|load1b, load2b|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"1", "2"|], [|"1", "2"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2"|])
                 }
               )
          }
        );
      testPromise
        "it allows priming the cache"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            IdentityLoader.prime "1" "1";
            let load1 = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            Js.Promise.all [|load1, load2|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"2"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"1", "2"|])
                 }
               )
          }
        );
      testPromise
        "it does not prime keys that already exist"
        (
          fun () => {
            let calls = [||];
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => {
                Js.Array.push strings calls;
                Js.Promise.resolve strings
              };
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            IdentityLoader.prime "1" "A";
            IdentityLoader.prime "1" "B";
            let load1 = IdentityLoader.load "1";
            let load2 = IdentityLoader.load "2";
            Js.Promise.all [|load1, load2|]
            |> Js.Promise.then_ (
                 fun loadedValues => {
                   expect calls |> toEqual [|[|"2"|]|];
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"A", "2"|])
                 }
               )
          }
        );
      testPromise
        "allows forcefully priming the cache"
        (
          fun () => {
            module IdentityLoaderImpl = {
              type value = string;
              let batchLoadFun strings => Js.Promise.resolve strings;
              let options: options = {batch: true, maxBatchSize: 2, cache: true};
            };
            module IdentityLoader = MakeDataloader IdentityLoaderImpl;
            IdentityLoader.prime "1" "A";
            let load1a = IdentityLoader.load "1";
            let load2a = IdentityLoader.load "2";
            Js.Promise.all [|load1a, load2a|]
            |> Js.Promise.then_ (
                 fun loadedValues =>
                   Js.Promise.resolve (expect loadedValues |> toEqual [|"A", "2"|])
               );
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
