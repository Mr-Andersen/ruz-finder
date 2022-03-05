#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash zip

unlink result; \
    nix-build -A ruz-finder.components.exes.ruz-finder || \
    exit 1
unlink result-mingw; \
    nix-build --out-link result-mingw -A projectCross.mingwW64.hsPkgs.ruz-finder.components.exes.ruz-finder && \
    zip -r build/ruz-finder-win64.zip result-mingw/bin/* || \
    exit 1
unlink result-docker; \
    nix-build --out-link result-docker docker.nix && \
    cp result/bin/ruz-finder build/ruz-finder || \
    exit 1
