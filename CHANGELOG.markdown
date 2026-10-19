# [0.3.1](https://github.com/RedPRL/asai/compare/0.3.0...0.3.1) (2024-02-23)

This is a minor release with several improvements, especially the debug mode to actively detect invalid ranges. OCaml 5.2.0 introduced backward-incompatible changes in ocaml/ocaml#12477 and thus we had to bump up the minimum OCaml version.

### Bug Fixes

- correct and enhance the debug mode ([#142](https://github.com/RedPRL/asai/issues/142)) ([db89139](https://github.com/RedPRL/asai/commit/db891396bfe7e79516b6215070b7536cac96b726))
- **Diagnostic:** use `Format.pp_infinity` in `string_of_text` for OCaml 5.2 ([#149](https://github.com/RedPRL/asai/issues/149)) ([b835fac](https://github.com/RedPRL/asai/commit/b835facfa5996ef7ff24fa78409e9c638d7c2de2))
- dim the carets ([#145](https://github.com/RedPRL/asai/issues/145)) ([08c7dc2](https://github.com/RedPRL/asai/commit/08c7dc2f923b78a15236a077a1029e8cc4626a40))

### Features

- comprehensive checking for ranges ([#147](https://github.com/RedPRL/asai/issues/147)) ([891d627](https://github.com/RedPRL/asai/commit/891d627f50709af5e210e4bfe8a97b377eda3119))
- **Explicator:** introduce debug mode ([#139](https://github.com/RedPRL/asai/issues/139)) ([9830591](https://github.com/RedPRL/asai/commit/983059128df785bde5c80aff5d30d81279079fbf))
- **Range:** re-introduce `split` ([7cdce93](https://github.com/RedPRL/asai/commit/7cdce936a18abafed132e3a0dc055d59ec17aef1))

# [0.3.0](https://github.com/RedPRL/asai/compare/0.2.0...0.3.0) (2023-11-03)

This is a major release with two notable changes:

1. New special `EOF` ranges were introduced to highlight the end of a file. The terminal renderer will show “‹EOF›” and highlight it if a special `EOF` range is used.
2. The terminal renderer was rewritten (again). Its visual appearance is almost identical to the previous one, except that the “faint” style replaces the gray coloring. Internally, it no longer uses `notty` (thus reducing the dependencies to a minimum), and it now automatically detects the environment variable `NO_COLOR`, following the specification at <https://no-color.org/>.

### BREAKING CHANGES

- **Range:** because of the new special `EOF` ranges, `split` is deprecated and will be removed soon; use `view` instead

### Bug Fixes

- **Reporter:** backtrace frames no longer inherit locations ([#128](https://github.com/RedPRL/asai/issues/128)) ([bbd7d0b](https://github.com/RedPRL/asai/commit/bbd7d0b747712ee54986747b136405820926cf9d))
- **Tty:** drop `notty` and use the "faint" style ([#135](https://github.com/RedPRL/asai/issues/135)) ([1988a5a](https://github.com/RedPRL/asai/commit/1988a5ae348d50472448673a00a37ed366eccabf))
- upgrade the dependency `algaeff` to 2.0.0 ([#132](https://github.com/RedPRL/asai/issues/132)) ([6e8b9a2](https://github.com/RedPRL/asai/commit/6e8b9a254cdebeb24ef1cfc862a876e100a794f9))

### Features

- **MinimumSigs:** new signatures to help library authors hide internal reporters ([#126](https://github.com/RedPRL/asai/issues/126)) ([058171c](https://github.com/RedPRL/asai/commit/058171cd1da4187d24614e643d9f8f0e9bb6c1d5))
- **Range:** allow special `EOF` ranges ([#125](https://github.com/RedPRL/asai/issues/125)) ([1b85fbd](https://github.com/RedPRL/asai/commit/1b85fbdf164b8d8da796c0fa084abdaf6386dc9b))
- **Tty:** detect `NO_COLOR` ([472f096](https://github.com/RedPRL/asai/commit/472f096a5cc1cbce53ab930e30f5867935b163d5))

# [0.2.0](https://github.com/RedPRL/asai/compare/0.1.1...0.2.0) (2023-10-23)

This is a major release with many backward incompatible changes.

The most significant change is the introduction of **StructuredReporter**. The library can thus be used in two different modes:

1. Use **Reporter** (as the old **Logger**): its API is biased towards free-form explanations.
2. Use **StructuredReporter:** its API is biased towards fully structured messages.

Both modes share the same type of **diagnostics**, and thus, an application using structured messages can adopt a library using free-form explanations and vice versa. Great care has been taken to make sure the two modes can work together. The current tutorial focuses on **Reporter** and free-form explanations because we think that mode is slightly easier to set up.

The second most significant change is that the TTY handler got an overhaul. The output is now more concise, informational, and beautiful.

Thanks to Mike Shulman for many valuable suggestions.

### BREAKING CHANGES

- **Span** is renamed to **Range**
- **Logger** is renamed to **Reporter** and **Reporter.Code** is renamed to **Reporter.Message**
- **Range:** the type `position` was changed to allow string (in-memory) sources
- **Diagnostic:** the type `diagnostic` was changed and `message` was renamed to `loctext`
- The LSP handler is separated out as a new package (not published yet)

### Bug Fixes

- **Diagnostic:** fix and improve `string_of_text`
  - ([#83](https://github.com/RedPRL/asai/issues/83)) ([e32adc5](https://github.com/RedPRL/asai/commit/e32adc5fbbd8cca6c2c0f633afa2ec1beb716f71))
  - ([b00d8cd](https://github.com/RedPRL/asai/commit/b00d8cd2eee9e51ea89fed8d0988d20fb7964e00))

### Features

- introduce **StructuredReporter** for fully structured messages
  - ([#97](https://github.com/RedPRL/asai/issues/97)) ([add65f8](https://github.com/RedPRL/asai/commit/add65f81ddc6a37734d32c4363d7abf45d96aa3c))
- **Range:** allow string (in-memory) sources
  - ([#90](https://github.com/RedPRL/asai/issues/90)) ([ae62741](https://github.com/RedPRL/asai/commit/ae62741933f6881a1da8f53be45249d347918321))
  - ([#101](https://github.com/RedPRL/asai/issues/101)) ([4b6819a](https://github.com/RedPRL/asai/commit/4b6819a289c514f92f0fbb06bee6ac5bd79a0962))
  - ([a944e66](https://github.com/RedPRL/asai/commit/a944e668ac16532059dff26064712c6300c3b15b))
- **Tty:** completely redesign the TTY output
  - ([#81](https://github.com/RedPRL/asai/issues/81)) ([9685bc9](https://github.com/RedPRL/asai/commit/9685bc92e0cc1fbff152814d9a7a340f14871be5))
  - ([#91](https://github.com/RedPRL/asai/issues/91)) ([07fe241](https://github.com/RedPRL/asai/commit/07fe24104404a33ea213c1910671d2fa4d14531c))
  - ([#93](https://github.com/RedPRL/asai/issues/93)) ([c3f0747](https://github.com/RedPRL/asai/commit/c3f07479254ad02823500262118de649666e6128))
  - ([39e7154](https://github.com/RedPRL/asai/commit/39e7154b746b5444bc97a24bdbb26e55a83bd2d8))
  - ([a9caca1](https://github.com/RedPRL/asai/commit/a9caca1f2e46e982d8fa56a69528df390dc6f1ef))
  - ([#114](https://github.com/RedPRL/asai/issues/114)) ([5d2066b](https://github.com/RedPRL/asai/commit/5d2066bf530c41e2a920b6258d905cffeeef9229))

# [0.1.1](https://github.com/RedPRL/asai/compare/0.1.0...0.1.1) (2023-10-01)

### Bug Fixes

- **OPAM:** remove the unused dependencies that caused trouble ([101b10f](https://github.com/RedPRL/asai/commit/101b10f784c7b62cd9f1324f5bb855667ae3eb26))
- **OPAM:** we actually need lsp 1.15 ([f715e7f](https://github.com/RedPRL/asai/commit/f715e7faee894fee169235d6626cde0cfbe39f60))
