stack build --install-ghc --stack-yaml stack-ghcjs.yaml \
  && cp static/* .stack-work/dist/x86_64-osx/Cabal-1.24.0.0_ghcjs/build/duet-web/duet-web.jsexe/ \
  && cp static/* .stack-work/dist/x86_64-osx/Cabal-1.24.0.0_ghcjs/build/duet-ide/duet-ide.jsexe/ \
  && cp static/* .stack-work/dist/x86_64-osx/Cabal-1.24.0.0_ghcjs/build/duet-ide-test/duet-ide-test.jsexe/ \
  && cp static/* .stack-work/dist/x86_64-osx/Cabal-1.24.0.0_ghcjs/build/duet-ide-doc/duet-ide-doc.jsexe/ \
  && cp static/* .stack-work/dist/x86_64-osx/Cabal-1.24.0.0_ghcjs/build/duet-ide-record/duet-ide-record.jsexe/
