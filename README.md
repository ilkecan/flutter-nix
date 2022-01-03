# flutter-nix
The project can be used to:
- create a development environment for your project that includes the
  dependencies required to build your app for the specified target platforms.
  Currently `linux`, `android` and `web` platforms are supported for this.
- build your Flutter app with Nix in a deterministic way. Currently `linux` and
  `web` platforms are supported for this.

The flake file provides a overlay that add an attribute set named `flutter-nix`
under `pkgs` with the following attributes:
### `mkShell`
A wrapper around "pkgs.mkShell" that includes the following additional
parameters:

| attribute name | default value | description |
| - | - | - |
| enableAll | `false` | Install dependencies for all supported target platforms |
| flutterNix | | Attribute set that includes parameters related to `flutter-nix` |
| android | | Attribute set that includes parameters related to Android platform |
| linux | | Attribute set that includes parameters related to Linux platform |
| web | | Attribute set that includes parameters related to web platform |

#### `mkShell.flutterNix`
| attribute name | default value | description |
| - | - | - |
| enable | `true` | Enable `flutter-nix` support, which currently means adding the translator to the development environment |

#### `mkShell.android`
| attribute name | default value | description |
| - | - | - |
| enable | `mkShell.enableAll \|\| false` | Add Android support for the created development environment |
| sdkPlatformApiLevels | `[ ]` | Specify the required Android SDK API levels (e.g. `[ 29 30 ]`)|

#### `mkShell.linux`
| attribute name | default value | description |
| - | - | - |
| enable | `mkShell.enableAll \|\| false` | Add Linux support for the created development environment |

#### `mkShell.web`
| attribute name | default value | description |
| - | - | - |
| enable | `mkShell.enableAll \|\| false` | Add web support for the created development environment |

Any other attribute is passed to `pkgs.mkDerivation` as is.

### `buildFlutterApp`
A wrapper around "pkgs.mkDerivation" that includes the following additional
parameters:

| attribute name | default value | description |
| - | - | - |
| src | **required** | Root directory of the Flutter project |
| platform | **required** | The target platform to build the app for,  see `flutter-nix.supportedPlatforms` |
| name | `name` attribute of the pubspec file | `pname` attribute of the created derivation |
| version | `version` attribute of the pubspec file | `version` attribute of the created derivation |
| flutterNixLockFile | `src + "/flutter-nix-lock.json" | The path of the flutter-nix lock file |
| filterSrc | `true` | Filter the given `src` with `nix-filter` to avoid unnecessary rebuilds |

Any other attribute is passed to `pkgs.mkDerivation` as is.

### `translator`
The command-line tool used to create the `flutter-nix-lock.json`. This file
contains information necessary to build the Flutter app inside the Nix sandbox.

### `supportedPlatforms`
Platforms supported by `buildFlutterApp`.

See [this example project](https://github.com/ilkecan/flutter-friendly_chat/blob/master/flake.nix) for how to use `flutter-nix`

## TODOS
- Add `android` support to `buildFlutterApp` (see:
  https://github.com/ilkecan/flutter-nix/issues/1#issuecomment-1000503410)
- Support non-flake evaluation
