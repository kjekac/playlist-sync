you need nix. then run with

```sh
nix run . -- \
  --playlist <spotify url> \
  --client-id <get this from api.spotify.com> \
  --client-secret <get this from api.spotify.com> \
  --username <soulseek username> \  # you can set these to literally anything
  --password <soulseek password> \  # or just use the same as in soulseekqt
  --downloads <target folder> \
  [ --shared <share folder> \       # optional but peers often require that you share stuff
  [ --debug ]                       # optional
```

you can also build everything with `nix build`, but then you have to manually put `slskd` on the `$PATH` before executing the binary. not recommended.

you can also hack on it with `nix develop`. after that you might want `cabal repl`.
