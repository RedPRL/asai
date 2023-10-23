# [0.2.0](https://github.com/RedPRL/asai/compare/0.1.1...0.2.0) (2023-10-23)

This is a major release with many backward incompatible changes.

The most significant change is the introduction of **StructuredReporter**. The library thus supports two different modes:

1. **Reporter** (as the old **Logger**): the API is biased towards free-form explanations.
2. **StructuredReporter:** the API is biased towards fully structured messages.

Both modes share the same type of **diagnostics**, and thus, an application using structured messages can adopt a library using free-form explanations and vice versa. Great care has been taken to make sure the two modes work together. The current tutorial focuses on **Reporter** and free-form explanations because we think that mode is slightly easier to set up.

The second most significant change is that the TTY handler got an overhaul. The output is now more concise, informational, and beautiful.

Thanks to Mike Shulman for many valuable suggestions.

### BREAKING CHANGES

- **Span** is renamed to **Range**
- **Logger** is renamed to **Reporter** and **Reporter.Code** is renamed to **Reporter.Message**
- **Diagnostic:** change the type of `extra_remarks` to a backward list ([fe0f60f](https://github.com/RedPRL/asai/commit/fe0f60fbb2a7f5c3aefd22c30f11d488ee83e855))
- **Range:** allow string sources
  ([#90](https://github.com/RedPRL/asai/issues/90)) ([ae62741](https://github.com/RedPRL/asai/commit/ae62741933f6881a1da8f53be45249d347918321))
  ([#101](https://github.com/RedPRL/asai/issues/101)) ([4b6819a](https://github.com/RedPRL/asai/commit/4b6819a289c514f92f0fbb06bee6ac5bd79a0962))

### Bug Fixes

- **Diagnostic:** fix and improve `string_of_text`
  ([#83](https://github.com/RedPRL/asai/issues/83)) ([e32adc5](https://github.com/RedPRL/asai/commit/e32adc5fbbd8cca6c2c0f633afa2ec1beb716f71))
  ([b00d8cd](https://github.com/RedPRL/asai/commit/b00d8cd2eee9e51ea89fed8d0988d20fb7964e00))
- separate the LSP handler out as a new package
  ([#105](https://github.com/RedPRL/asai/issues/105)) ([a2b3da8](https://github.com/RedPRL/asai/commit/a2b3da8f233f6475684120129138ee4c7d3cd5a3))

### Features

- introduce **StructuredReporter** for fully structured messages
  ([#97](https://github.com/RedPRL/asai/issues/97)) ([add65f8](https://github.com/RedPRL/asai/commit/add65f81ddc6a37734d32c4363d7abf45d96aa3c))
- **Range:** add `to_lex_position` and `to_lex_range`
  ([#121](https://github.com/RedPRL/asai/issues/121)) ([1836e38](https://github.com/RedPRL/asai/commit/1836e38b8fac53ecd32524744b726a85a0041da7))
- **Tty:** completely redesign the TTY output
  ([#81](https://github.com/RedPRL/asai/issues/81)) ([9685bc9](https://github.com/RedPRL/asai/commit/9685bc92e0cc1fbff152814d9a7a340f14871be5))
  ([#91](https://github.com/RedPRL/asai/issues/91)) ([07fe241](https://github.com/RedPRL/asai/commit/07fe24104404a33ea213c1910671d2fa4d14531c))
  ([#93](https://github.com/RedPRL/asai/issues/93)) ([c3f0747](https://github.com/RedPRL/asai/commit/c3f07479254ad02823500262118de649666e6128))
  ([39e7154](https://github.com/RedPRL/asai/commit/39e7154b746b5444bc97a24bdbb26e55a83bd2d8))
  ([a9caca1](https://github.com/RedPRL/asai/commit/a9caca1f2e46e982d8fa56a69528df390dc6f1ef))
  ([#114](https://github.com/RedPRL/asai/issues/114)) ([5d2066b](https://github.com/RedPRL/asai/commit/5d2066bf530c41e2a920b6258d905cffeeef9229))

# [0.1.1](https://github.com/RedPRL/asai/compare/0.1.0...0.1.1) (2023-10-01)

### Bug Fixes

- **OPAM:** remove the unused dependencies that caused trouble ([101b10f](https://github.com/RedPRL/asai/commit/101b10f784c7b62cd9f1324f5bb855667ae3eb26))
- **OPAM:** we actually need lsp 1.15 ([f715e7f](https://github.com/RedPRL/asai/commit/f715e7faee894fee169235d6626cde0cfbe39f60))
