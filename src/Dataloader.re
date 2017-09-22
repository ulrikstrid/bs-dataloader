type options = {
  batch: bool,
  maxBatchSize: int,
  cache: bool
};

module type Impl = {
  type value;
  /*
   * A Function, which when given an Array of keys, returns a Promise of an Array
   * of values or Errors.
   */
  let batchLoadFun: array string => Js.Promise.t (array value);
  let options: options;
};

module Dataloader (Impl: Impl) => {
  let shouldCache = Impl.options.cache;
  let shouldBatch = Impl.options.batch;
  let maxBatchSize = Impl.options.maxBatchSize;
  let batchLoadFun = Impl.batchLoadFun;
  let promiseCache: Js.Dict.t (Js.Promise.t Impl.value) = Js.Dict.empty ();
  let queue = [||];
  let resolvedPromise = Js.Promise.resolve ();
  let enqueuePostPromiseJob fn => {
    resolvedPromise
    |> Js.Promise.then_ (
         fun _ => {
           let _ = Js.Global.setTimeout fn 1;
           Js.Promise.resolve ()
         }
       );
    ()
  };
  let dispatchQueueBatch queueSlice => {
    let keys = Js.Array.map (fun (key, _, _) => key) queueSlice;
    let _ =
      batchLoadFun keys
      |> Js.Promise.then_ (
           fun values => {
             let _ =
               queueSlice
               |> Js.Array.mapi (
                    fun (key, resolve, reject) index => {
                      let value = values.(index);
                      (key, resolve, reject, value)
                    }
                  )
               |> Js.Array.forEach (fun (_key, resolve, _reject, value) => resolve value);
             Js.Promise.resolve ()
           }
         );
    ()
  };
  let rec dispatchQueue () => {
    let queueSize = Js.Array.length queue;
    let queueIsLarger = maxBatchSize < queueSize;
    let sliceTo = queueIsLarger ? maxBatchSize : queueSize;
    dispatchQueueBatch (Js.Array.slice 0 sliceTo queue);
    Js.Array.removeCountInPlace pos::0 count::sliceTo queue;
    ()
  };
  let doLoad key => {
    let promise =
      Js.Promise.make (
        fun ::resolve ::reject => {
          let _ = Js.Array.push (key, resolve, reject) queue;
          if (Js.Array.length queue == 1) {
            if shouldBatch {
              {}
            } else {
              {}
            };
            ()
          } else {
            dispatchQueue ()
          }
        }
      );
    if shouldCache {
      Js.Dict.set promiseCache key promise;
      promise
    } else {
      promise
    }
  };
  let load (key: string) =>
    if shouldCache {
      let cachedResult = Js.Dict.get promiseCache key;
      switch cachedResult {
      | Some result => result
      | None => doLoad key
      }
    } else {
      doLoad key
    };
};
