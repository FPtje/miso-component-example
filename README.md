# Scalable apps using components

Let's say you've tried out [miso](http://haskell-miso.org/). Awesome! Now you want to make a bigger app, but you probably don't want to have the entire app in *one* big `Model`, *one* huge set of `Action`s and a `view` function larger than the Eiffel tower. You'll want to divide things up in components. Each of those components should:

- Be reusable
- Have its own state (`Model`)
- Hide its complexity from its parents, yet...
- Communicate key events to its parents
- Be able to embed its own components

## The Pattern
See [`Button.hs`](src/Button.hs), which is the component, and [`Main.hs`](src/Main.hs), which embeds two buttons.

The `Button` module has its own `Model`, `Action`, `updateModel` and `viewModel`. When looking at `updateModel` and `viewModel`, we see that their definitions are slightly different than we're used to.

| Function  | Expected  | Actual |
| --------- | --------- | ------ |
| `updateModel` | `Action -> Model -> Effect Action Model` | `PublicActions action -> Action -> Transition Model action` |
| `viewModel` | `Model -> View Action` | `PublicActions action -> Model -> View action` |

The `PublicActions action` is defined above in the file. The component decides which actions are interesting for its parent through this data structure. The `ParentActions` record has a field for every such `Action`. This allows the parent to listen to the important actions, while not having to bother with the ones that are only meant for the component itself.

## Running the example

From this directory, run:

```bash
nix-shell --run 'cabal configure && cabal build'
```

Then open `dist/build/main/main.jsexe/index.html`


Alternatively:

```bash
nix-build
```

Then open `result/bin/main.jsexe/index.html`
