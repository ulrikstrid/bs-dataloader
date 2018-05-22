open Jest;

describe("Can handle errors", () => {
  module IdentityLoaderImpl = {
    type value = string;
    type key = string;
    let batchLoadFun = (strings: array(string)) => {
      let promises =
        Array.map(
          stringIdentity =>
            stringIdentity == "2" ?
              Js.Promise.resolve(Belt.Result.Error(Not_found)) :
              Js.Promise.resolve(Belt.Result.Ok(stringIdentity)),
          strings,
        );
      Js.Promise.all(promises);
    };
    let options: DataLoader.options = {
      batch: true,
      maxBatchSize: 256,
      cache: true,
    };
  };
  testPromise("if a error is returned it will return the error", () => {
    open Expect;
    open! Expect.Operators;
    module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
    let load1 = IdentityLoader.load("1");
    let load2 =
      IdentityLoader.load("2")
      |> Js.Promise.catch((_) => Js.Promise.resolve("error"));
    Js.Promise.all([|load1, load2|])
    |> Js.Promise.then_(loadedValues =>
         Js.Promise.resolve(
           expect(loadedValues) |> toEqual([|"1", "error"|]),
         )
       );
  });
});

describe("Does not cache errors", () => {
  module IdentityLoaderImpl = {
    type value = string;
    type key = string;
    let calls = ref(1);
    let batchLoadFun = (strings: array(string)) => {
      let promises =
        Array.map(
          stringIdentity =>
            calls^ == 1 ?
              Js.Promise.resolve(Belt.Result.Error(Not_found)) :
              Js.Promise.resolve(Belt.Result.Ok(stringIdentity)),
          strings,
        );
      calls := calls^ + 1;
      Js.Promise.all(promises);
    };
    let options: DataLoader.options = {
      batch: false,
      maxBatchSize: 256,
      cache: true,
    };
  };
  module IdentityLoader = DataLoader.Make(IdentityLoaderImpl);
  testPromise("returns a error the first time", () => {
    open Expect;
    open! Expect.Operators;
    IdentityLoader.load("1")
    |> Js.Promise.catch((_) => Js.Promise.resolve("error"))
    |> Js.Promise.then_(loadedValue =>
         Js.Promise.resolve(expect(loadedValue) |> toEqual("error"))
       );
  });
  testPromise("returns a value the second time", () => {
    open Expect;
    open! Expect.Operators;
    IdentityLoader.load("1")
    |> Js.Promise.catch((_) => Js.Promise.resolve("error"))
    |> Js.Promise.then_(loadedValue =>
         Js.Promise.resolve(expect(loadedValue) |> toEqual("1"))
       );
  });
});
