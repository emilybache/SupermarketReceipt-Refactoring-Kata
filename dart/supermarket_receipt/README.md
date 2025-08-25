# Supermarket Receipt in Dart

## Setup

* Get Dart SDK from https://dart.dev/get-dart
* Install (unpack, set environment variable)
* When using in VSCode, install the Dart extension (dartcode.org)
* In VSCode, open the project directory: supermarket\_receipt

## Run test in console

* `cd supermarket_receipt`
* `dart run test` or `dart run test/supermarket/model/supermarket_test.dart`

## Run with coverage

* In VSCode, install the extension Coverage Gutters
* `cd supermarket_receipt`
* `dart run coverage:test_with_coverage`

## Run one single test with coverage

* `dart test --coverage=coverage --plain-name "threeForTwo" test/supermarket/model/supermarket_test.dart`
* `dart run coverage:format_coverage --packages=.dart_tool/package_config.json --report-on=lib --in=coverage --out=coverage/lcov.info --lcov`

## Display coverage in VS Code

Open Command Palette (Ctrl + Shift + P), select Coverage Gutters: Display Coverage (Ctrl + Shift + 7)
In user settings.json, add "coverage-gutters.showLineCoverage": true
