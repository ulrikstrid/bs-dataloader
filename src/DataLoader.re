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

let firstNInQueueToArray queue numberOfValues => {
  let queueLength = Queue.length queue;
  let maxValues = queueLength > numberOfValues ? numberOfValues : queueLength;
  Array.init maxValues (fun _ => Queue.pop queue)
};

module Make (Impl: Impl) => {
  let shouldCache = Impl.options.cache;
  let shouldBatch = Impl.options.batch;
  let maxBatchSize = Impl.options.maxBatchSize;
  let batchLoadFun = Impl.batchLoadFun;
  let promiseCache: Hashtbl.t string (Js.Promise.t Impl.value) = Hashtbl.create 10;
  let queue:
    Queue.t (string, Js.Internal.fn [ | `Arity_1 'a] unit, Js.Internal.fn [ | `Arity_1 exn] unit) =
    Queue.create ();
  let dispatchQueueBatch
      (
        queueSlice:
          array (
            string,
            Js.Internal.fn [ | `Arity_1 'a] unit,
            Js.Internal.fn [ | `Arity_1 exn] unit
          )
      ) => {
    let keys = Array.map (fun (key, _, _) => key) queueSlice;
    let _ =
      batchLoadFun keys
      |> Js.Promise.then_ (
           fun values => {
             let _ =
               queueSlice
               |> Array.iteri (
                    fun index
                        (
                          (key, resolve, reject): (
                            string,
                            Js.Internal.fn [ | `Arity_1 'a] unit,
                            Js.Internal.fn [ | `Arity_1 exn] unit
                          )
                        ) =>
                      resolve values.(index)
                  );
             Js.Promise.resolve ()
           }
         );
    ()
  };
  let rec dispatchQueue () => {
    let queueSize = Queue.length queue;
    let queueIsLarger = maxBatchSize < queueSize;
    if (queueSize > 0) {
      dispatchQueueBatch (firstNInQueueToArray queue maxBatchSize);
      dispatchQueue ()
    } else {
      ()
    }
  };
  let load (key: string) =>
    if (shouldCache && Hashtbl.mem promiseCache key) {
      Hashtbl.find promiseCache key
    } else {
      let promise =
        Js.Promise.make (
          fun ::resolve ::reject => {
            let _ = Queue.push (key, resolve, reject) queue;
            if (Queue.length queue == 1) {
              if shouldBatch {
                enqueuePostPromiseJob dispatchQueue
              } else {
                dispatchQueue ()
              };
              ()
            } else {
              dispatchQueue ()
            }
          }
        );
      if shouldCache {
        Hashtbl.add promiseCache key promise;
        promise
      } else {
        promise
      }
    };
  let loadMany keys => Js.Promise.all (Array.map load keys);
  let clear key => Hashtbl.remove promiseCache key;
  let clearAll () => Hashtbl.clear promiseCache;
  let prime key value =>
    if (Hashtbl.mem promiseCache key) {
      ()
    } else {
      Hashtbl.add promiseCache key (Js.Promise.resolve value)
    };
};
