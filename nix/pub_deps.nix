{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

let
  fetch = name: version: hash: fetchzip {
    name = "${name}-${version}";
    url = "https://pub.dartlang.org/packages/${name}/versions/${version}.tar.gz";
    sha256 = hash;
  };
in
[
  (fetch "async" "2.8.1" "0ycm71qvkb9ywrqgf2hqp28v3r0kbjbmvqhb1scfn39xy6raw7sb")
  (fetch "boolean_selector" "2.1.0" "1rk7z1pa4yrnbdw5djqykx4w9lzpfdkb40z8zvnqzmr9mbkd60jx")
  (fetch "characters" "1.1.0" "0rs85ak0rj35d12qj498qilrmrkvrcls38fvm810x1y3rlrxfzq1")
  (fetch "charcode" "1.3.1" "0lidr1l373bl8lhcz9b3wahxs2f7wl98n92h94cf7zzdxy4c37id")
  (fetch "clock" "1.1.0" "0m7cwaj0z2ip8mkbhf4d0nnk0mn06lxg4nc428kdp1rs5f86701k")
  (fetch "collection" "1.15.0" "1ff5qakfi33dmd7ccrnj2rqf7id97bhxhdf61026jp4ynkb1gi4v")
  (fetch "cupertino_icons" "1.0.4" "1n50mqqvimfr05xyazmkvww7819ibfqn6n3cv8nxmzf81pcvyrcf")
  (fetch "fake_async" "1.2.0" "1dlm5zra3hfblib98s4fbyrl2jp1kn063w5x298bmg5a0j63zwar")
  (fetch "flutter_lints" "1.0.4" "0pj80mkyjq1dk3kbx1ylv8cjakzx9zwsa4bvpn3wpz5r9kyakyfy")
  (fetch "lints" "1.0.1" "1xyn9xpzwfw1f0mp03pyvspcphkinhzawkgp5lwmi7p15mv1vgz2")
  (fetch "matcher" "0.12.10" "1vlarbza5hkzkn32c5h8jny4v0rjbgi095w37livqdvrp9nhwc99")
  (fetch "meta" "1.7.0" "1z8sx23l9jn2ickq3z63pqpb6k9y6gbnnvj9200c6v7m3cvd7jbv")
  (fetch "path" "1.8.0" "0hl7jv712paww2r8z3f0cbxdfzhpashpxxkdx3pmv2gdlb8m3596")
  (fetch "source_span" "1.8.1" "17zqqpazwm21hawkcz4jfhnr207qrgbyj95mmr4fckgm19za78lr")
  (fetch "stack_trace" "1.10.0" "01lyk22h1zxr3a39wmai9lvxykia0y8g7lc2kcpnjypxhf4g0sbl")
  (fetch "stream_channel" "2.1.0" "0591c3bpjipx4jlzxwsyspmgpjryh6psl1ks38jkcxp3pffkybsv")
  (fetch "string_scanner" "1.1.0" "14zygj9v3hq1ykpksknghnaa6gxp501kca4576vlybrvahj3d129")
  (fetch "term_glyph" "1.2.0" "10i2vjhjrqssvj1j7jy7f8iijiq46bjib40rd9lb5f15kixly3q4")
  (fetch "test_api" "0.4.2" "1i0p2njjrd88lxn3lw7yf1dbdrpmlr1w9avbfpcxiwsxkipxb8vv")
  (fetch "typed_data" "1.3.0" "03qnxdi3dgxvkdfg3s7m4dnsrgrz9r657wzqxia0j3nslz3rgn5f")
  (fetch "vector_math" "2.1.0" "1hfm9hngiy6vlhjm054qyiaxhvbngxfhlx56nlx1xmkmjql9bn3v")
]
