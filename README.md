# Antipode Globe

A ClojureScript webapp for seeing how the far side of Earth maps onto the near side.

The globe draws two outline layers:

- white country outlines for the normal Earth view
- cyan country outlines transformed through the antipode formula, so opposite-side geography is overlaid on the visible hemisphere

## Run

```sh
npm install
npm run dev
```

Then open [http://localhost:8020](http://localhost:8020).

## Build

```sh
npm run build
```

## Deploy

Pushes to `main` deploy automatically through GitHub Actions. In the GitHub repository settings, set Pages to use **GitHub Actions** as the build and deployment source.
