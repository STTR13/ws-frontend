# Work Station Frontend
> 🌳  built in [elm](https://elm-lang.org) using [elm-spa](https://elm-spa.dev) to handle routing and [elm-graphql](https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest) for communication with the backend.

## dependencies

This project requires the latest LTS version of [Node.js](https://nodejs.org/)

```bash
npm install -g elm elm-spa
```

When [ws-backend](https://github.com/42-AI/ws-backend)'s graphql schema evolve you need to run `npm run regen-graphql`, then run `elm-spa server` and follow compiler errors to fix the changes.


## running locally

```bash
elm-spa server  # starts this app at http:/localhost:1234
```

### other commands

```bash
elm-spa add    # add a new page to the application
elm-spa build  # production build
elm-spa watch  # runs build as you code (without the server)
```

## learn more

You can learn more at [elm-spa.dev](https://elm-spa.dev)
