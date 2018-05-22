open Jest;

describe("Primary API", () => {
  open Expect;
  open! Expect.Operators;
  module IdentityLoaderImpl = {
    type value = string;
    type key = string;
    let batchLoadFun = strings =>
      Js.Promise.resolve(
        Array.map(string => Belt.Result.Ok(string), strings),
      );
    let options: DataLoader.options = {
      batch: false,
      maxBatchSize: 256,
      cache: true,
    };
  };
  testPromise("it builds a really simple data loader", () => {
    module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
    IdentityLoader.load("1")
    |> Js.Promise.then_(loadedVal =>
         Js.Promise.resolve(expect(loadedVal) |> toBe("1"))
       );
  });
  testPromise("it supports loading multiple keys in one call", () => {
    module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
    IdentityLoader.loadMany([|"1", "2"|])
    |> Js.Promise.then_(loadedVal =>
         Js.Promise.resolve(expect(loadedVal) |> toEqual([|"1", "2"|]))
       );
  });
  testPromise("it batches multiple requests", () => {
    module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
    Js.Promise.all([|IdentityLoader.load("1"), IdentityLoader.load("2")|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(expect(loadedValues) |> toEqual([|"1", "2"|]))
       );
  });
  testPromise("it batches multiple requests with max batch sizes", () => {
    let calls = [||];
    module IdentityLoaderBatchImpl = {
      type value = string;
      type key = string;
      let batchLoadFun = strings => {
        let _ = Js.Array.push(strings, calls);
        Js.Promise.resolve(
          Array.map(string => Belt.Result.Ok(string), strings),
        );
      };
      let options: DataLoader.options = {
        batch: true,
        maxBatchSize: 2,
        cache: true,
      };
    };
    module IdentityLoader = DataLoader.Make(IdentityLoaderBatchImpl);
    let load1 = IdentityLoader.load("1");
    let load2 = IdentityLoader.load("2");
    let load3 = IdentityLoader.load("3");
    Js.Promise.all([|load1, load2, load3|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "2", "3"|], [|[|"1", "2"|], [|"3"|]|])),
         )
       );
  });
  testPromise("coalesces identical requests", () => {
    let calls = [||];
    module IdentityLoaderBatchImpl = {
      type value = string;
      type key = string;
      let batchLoadFun = strings => {
        let _ = Js.Array.push(strings, calls);
        Js.Promise.resolve(
          Array.map(string => Belt.Result.Ok(string), strings),
        );
      };
      let options: DataLoader.options = {
        batch: true,
        maxBatchSize: 2,
        cache: true,
      };
    };
    module IdentityLoader = DataLoader.Make(IdentityLoaderBatchImpl);
    let promise1a = IdentityLoader.load("1");
    let promise1b = IdentityLoader.load("1");
    Js.Promise.all([|promise1a, promise1b|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "1"|], [|[|"1"|]|])),
         )
       );
  });
});

describe("caches repeated requests", () => {
  open Expect;
  open! Expect.Operators;
  let calls = [||];
  module IdentityLoaderBatchImpl = {
    type value = string;
    type key = string;
    let batchLoadFun = strings => {
      let _ = Js.Array.push(strings, calls);
      Js.Promise.resolve(
        Array.map(string => Belt.Result.Ok(string), strings),
      );
    };
    let options: DataLoader.options = {
      batch: true,
      maxBatchSize: 2,
      cache: true,
    };
  };
  module IdentityLoader = DataLoader.Make(IdentityLoaderBatchImpl);
  testPromise("initial call", () => {
    let load1a = IdentityLoader.load("1");
    let load2 = IdentityLoader.load("2");
    Js.Promise.all([|load1a, load2|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "2"|], [|[|"1", "2"|]|])),
         )
       );
  });
  testPromise("repeted requests", () => {
    let load1b = IdentityLoader.load("1");
    let load3 = IdentityLoader.load("3");
    Js.Promise.all([|load1b, load3|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "3"|], [|[|"1", "2"|], [|"3"|]|])),
         )
       );
  });
});

describe("clear single value", () => {
  open Expect;
  open! Expect.Operators;
  let calls = [||];
  module IdentityLoaderImpl = {
    type value = string;
    type key = string;
    let batchLoadFun = strings => {
      let _ = Js.Array.push(strings, calls);
      Js.Promise.resolve(
        Array.map(string => Belt.Result.Ok(string), strings),
      );
    };
    let options: DataLoader.options = {
      batch: true,
      maxBatchSize: 2,
      cache: true,
    };
  };
  module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
  testPromise("can get a value", () => {
    let load1a = IdentityLoader.load("1");
    let load2a = IdentityLoader.load("2");
    Js.Promise.all([|load1a, load2a|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "2"|], [|[|"1", "2"|]|])),
         )
       );
  });
  testPromise("clears single value in loader", () => {
    IdentityLoader.clear("1");
    let load1b = IdentityLoader.load("1");
    let load2b = IdentityLoader.load("2");
    Js.Promise.all([|load1b, load2b|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "2"|], [|[|"1", "2"|], [|"1"|]|])),
         )
       );
  });
});

describe("clear all values", () => {
  open Expect;
  open! Expect.Operators;
  let calls = [||];
  module IdentityLoaderImpl = {
    type value = string;
    type key = string;
    let batchLoadFun = strings => {
      let _ = Js.Array.push(strings, calls);
      Js.Promise.resolve(
        Array.map(string => Belt.Result.Ok(string), strings),
      );
    };
    let options: DataLoader.options = {
      batch: true,
      maxBatchSize: 2,
      cache: true,
    };
  };
  module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
  testPromise("loads initial data", () => {
    let load1a = IdentityLoader.load("1");
    let load2a = IdentityLoader.load("2");
    Js.Promise.all([|load1a, load2a|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "2"|], [|[|"1", "2"|]|])),
         )
       );
  });
  testPromise("clears all values in loader", () => {
    IdentityLoader.clearAll();
    let load1b = IdentityLoader.load("1");
    let load2b = IdentityLoader.load("2");
    Js.Promise.all([|load1b, load2b|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "2"|], [|[|"1", "2"|], [|"1", "2"|]|])),
         )
       );
  });
});

describe("Priming the cache", () => {
  open Expect;
  open! Expect.Operators;
  testPromise("it allows priming the cache", () => {
    let calls = [||];
    module IdentityLoaderImpl = {
      type value = string;
      type key = string;
      let batchLoadFun = strings => {
        let _ = Js.Array.push(strings, calls);
        Js.Promise.resolve(
          Array.map(string => Belt.Result.Ok(string), strings),
        );
      };
      let options: DataLoader.options = {
        batch: true,
        maxBatchSize: 2,
        cache: true,
      };
    };
    module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
    IdentityLoader.prime("1", "1");
    let load1 = IdentityLoader.load("1");
    let load2 = IdentityLoader.load("2");
    Js.Promise.all([|load1, load2|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect((loadedValues, calls))
           |> toEqual(([|"1", "2"|], [|[|"2"|]|])),
         )
       );
  });
  testPromise("it does not prime keys that already exist", () => {
    module IdentityLoaderImpl = {
      type value = string;
      type key = string;
      let batchLoadFun = strings =>
        Js.Promise.resolve(
          Array.map(string => Belt.Result.Ok(string), strings),
        );
      let options: DataLoader.options = {
        batch: false,
        maxBatchSize: 256,
        cache: true,
      };
    };
    module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
    IdentityLoader.prime("1", "A");
    IdentityLoader.prime("1", "B");
    let load1 = IdentityLoader.load("1");
    let load2 = IdentityLoader.load("2");
    Js.Promise.all([|load1, load2|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(expect(loadedValues) |> toEqual([|"A", "2"|]))
       );
  });
});

describe("Priming the cache by force", () => {
  open Expect;
  open! Expect.Operators;
  module IdentityLoaderImpl = {
    type value = string;
    type key = string;
    let batchLoadFun = strings =>
      Js.Promise.resolve(
        Array.map(string => Belt.Result.Ok(string), strings),
      );
    let options: DataLoader.options = {
      batch: false,
      maxBatchSize: 256,
      cache: true,
    };
  };
  module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
  testPromise("allows priming the cache", () => {
    IdentityLoader.prime("1", "A");
    let load1a = IdentityLoader.load("1");
    let load2a = IdentityLoader.load("2");
    Js.Promise.all([|load1a, load2a|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(expect(loadedValues) |> toEqual([|"A", "2"|]))
       );
  });
  testPromise("allows forcefully priming the cache", () => {
    IdentityLoader.clear("1");
    IdentityLoader.prime("1", "X");
    IdentityLoader.clear("2");
    IdentityLoader.prime("2", "Y");
    let load1b = IdentityLoader.load("1");
    let load2b = IdentityLoader.load("2");
    Js.Promise.all([|load1b, load2b|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(expect(loadedValues) |> toEqual([|"X", "Y"|]))
       );
  });
});
