#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash zip
#! bash +x

linux_app=build/ruz-finder
docker_image=build/docker-image.tar.gz

unlink result
nix-build -A ruz-finder.components.exes.ruz-finder || exit 1
sudo cp result/bin/ruz-finder $linux_app && sudo chown $USER:users $linux_app || exit 2

unlink result-mingw
nix-build --out-link result-mingw -A projectCross.mingwW64.hsPkgs.ruz-finder.components.exes.ruz-finder || exit 3
zip -r build/ruz-finder-win64.zip result-mingw/bin/* || exit 4

unlink result-docker
nix-build --out-link result-docker docker.nix || exit 5
sudo cp result-docker $docker_image && sudo chown $USER:users $docker_image || exit 6
