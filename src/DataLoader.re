type options = {
  batch: bool,
  maxBatchSize: int,
  cache: bool,
};

module type Impl = {
  type key;
  type value;
  /*
   * A Function, which when given an Array of keys, returns a Promise of an Array
   * of values or Errors.
   */
  let batchLoadFun:
    array(key) => Js.Promise.t(array(Belt.Result.t(value, exn)));
  let options: options;
};

let resolvedPromise = Js.Promise.resolve();

let enqueuePostPromiseJob = fn => {
  let _ =
    resolvedPromise
    |. Js.Promise.then_(
         (_) => {
           let _ = Js.Global.setTimeout(fn, 1);
           Js.Promise.resolve();
         },
         _,
       );
  ();
};

let firstNInQueueToArray = (queue, numberOfValues) => {
  let queueLength = Queue.length(queue);
  let maxValues = queueLength > numberOfValues ? numberOfValues : queueLength;
  Belt.Array.makeBy(maxValues, (_) => Queue.pop(queue));
};

module Make = (Impl: Impl) => {
  let shouldCache = Impl.options.cache;
  let shouldBatch = Impl.options.batch;
  let maxBatchSize = Impl.options.maxBatchSize;
  let batchLoadFun = Impl.batchLoadFun;
  let promiseCache: Hashtbl.t(Impl.key, Js.Promise.t(Impl.value)) =
    Hashtbl.create(10);
  let queue = Queue.create();
  let clear = key => Hashtbl.remove(promiseCache, key);
  let clearAll = () => Hashtbl.clear(promiseCache);
  let prime = (key, value) =>
    if (Hashtbl.mem(promiseCache, key)) {
      ();
    } else {
      Hashtbl.add(promiseCache, key, Js.Promise.resolve(value));
    };
  let dispatchQueueBatch = queueSlice => {
    let keys = Belt.Array.map(queueSlice, ((key, _, _)) => key);
    batchLoadFun(keys)
    |. Js.Promise.then_(
         values => {
           queueSlice
           |. Belt.Array.forEachWithIndex((index, (key, resolve, reject)) =>
                switch (values[index]) {
                | Belt.Result.Ok(value) => resolve(. value)
                | Belt.Result.Error(err) =>
                  clear(key);
                  reject(. err);
                }
              )
           |. ignore;
           Js.Promise.resolve();
         },
         _,
       )
    |. ignore;
    ();
  };
  let rec dispatchQueue = () =>
    if (Queue.is_empty(queue) == false) {
      dispatchQueueBatch(firstNInQueueToArray(queue, maxBatchSize));
      dispatchQueue();
    } else {
      ();
    };
  let addToQueue = item => Queue.push(item, queue);
  let load = (key: Impl.key) =>
    if (shouldCache && Hashtbl.mem(promiseCache, key)) {
      Hashtbl.find(promiseCache, key);
    } else {
      let promise =
        Js.Promise.make((~resolve, ~reject) => {
          let _ = addToQueue((key, resolve, reject));
          if (Queue.length(queue) == 1) {
            if (shouldBatch) {
              enqueuePostPromiseJob(dispatchQueue);
            } else {
              dispatchQueue();
            };
            ();
          } else {
            dispatchQueue();
          };
        });
      if (shouldCache) {
        Hashtbl.add(promiseCache, key, promise);
        promise;
      } else {
        promise;
      };
    };
  let loadMany = keys => Js.Promise.all(Belt.Array.map(keys, load));
};
