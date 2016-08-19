rm -rf out || exit 0;

npm install -g elm

elm-package install --yes
elm-make --yes

mkdir out
elm-make Main.elm --output out/index.html --yes
cd out
