{
  flutter,
}:

let
  createCacheStamp = { name, from ? name }: ''
    ln -s \
      "${flutter.unwrapped}/bin/internal/${from}.version" \
      "${name}.stamp"
  '';
in
{
  inherit
    createCacheStamp
  ;
}
