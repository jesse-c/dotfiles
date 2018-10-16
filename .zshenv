export GOPATH=~/
export PATH="/usr/local/sbin:/Users/jesse/.asdf/installs/rust/nightly/bin:$GOPATH/bin:$PATH"
# https://stackoverflow.com/questions/35898734/pip-installs-packages-successfully-but-executables-not-found-from-command-line/48380776#48380776
python -m site &> /dev/null && PATH="$PATH:`python -m site --user-base`/bin"
python2 -m site &> /dev/null && PATH="$PATH:`python2 -m site --user-base`/bin"
