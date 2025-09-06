you need nix. then run with

```sh
nix run . -- \
  --playlist <spotify url> \
  --client-id <get this from api.spotify.com> \
  --client-secret <get this from api.spotify.com> \
  --username <soulseek username> \
  --password <soulseek password> \
  --downloads <target folder> \
  [ --shared <share folder> \ # optional but peers often require that you share stuff
  [ --debug ]                 # optional
```

or just build with `nix build`.

you can also hack on it with `nix develop`. after that you might want `cabal repl`.
