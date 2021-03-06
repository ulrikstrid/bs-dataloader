# bs-dataloader

[![CircleCI](https://circleci.com/gh/ulrikstrid/bs-dataloader.svg?style=svg)](https://circleci.com/gh/ulrikstrid/bs-dataloader)

This is a rewrite of [dataloader js lib](https://github.com/facebook/dataloader) in reasonml for use with BuckleScript. I have a goal to make it work with native OCaml in the future but I still rely on some JavaScript structures.

## Usage

```ocaml
module UserLoaderImpl = {
  type value = user;
  type key = userId;

  /* The batchLoadFun needs to wrap the returned values in Js.Result.t so that we can reject each promise instead of all */
  let batchLoadFun = userIds => batchGetUsers(userIds);
  let options: options = {batch: true, maxBatchSize: 256, cache: true};
};

module UserLoader = DataLoader.Make(UserLoaderImpl);

UserLoader.load("1")
|> Js.Promise.then_(
  user => {
    displayUser user;
    Js.Promise.resolve ();
  }
);

/* elsewhere in your app */

UserLoader.load("2")
|> Js.Promise.then_(
  user => {
    interactWithUser user;
    Js.Promsie.resolve();
  }
);
```

The example above will load both users at the same time with 1 request. If we later need one of the loaded users again we will get a cached version of the promise, if you need a new version from the server you can clear the key.

```ocaml
updateUser(~userId, ~userPatch)
|> Js.Promise.then_(_ => Js.Promise.resolve(UserLoader.clear userId))
|> Js.Promise.then_(_ => UserLoader.load("1"))
|> Js.Promise.then_(user => {
  displayUser user;
  Js.Promise.resolve ();
});
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
