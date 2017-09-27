# bs-dataloader

This is a rewrite of [dataloader js lib](https://github.com/facebook/dataloader) in reasonml for use with BuckleScript. I have a goal to make it work with native OCaml in the future but I still rely on some JavaScript structures.

## Usage

```re
module UserLoaderImpl = {
  type value = user;
  let batchLoadFun userIds => batchGetUsers userIds;
  let options: options = {batch: true, maxBatchSize: 256, cache: true};
};

module UserLoader = DataLoader.Make UserLoaderImpl;

UserLoader.load "1"
|> Js.Promise.then_ (
  fun user => displayUser user;
  Js.Promise.resolve ()
);

/* elsewhere in your app */

UserLoader.load "2"
|> Js.Promise.then_ (
  fun user => interactWithUser user;
  Js.Promsie.resolve()
);
```

The example above will load both users at the same time with 1 request.
If we later need one of the loaded users again we will get a cached version of the promise, if you need a new version from the server you can clear the key.

```re
updateUser ::userId ::userPatch
|> Js.Promise.then_ (fun _ => Js.Promise.resolve (UserLoader.clear userId))
|> Js.Promise.then_ (fun _ => UserLoader.load "1")
|> Js.Promise.then_ (
  fun user => displayUser user;
  Js.Promise.resolve ()
)
```

## Development

### Build
```
npm run build
```

### Build + Watch

```
npm run watch
```

### Test

```
npm run test
```

### Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically
