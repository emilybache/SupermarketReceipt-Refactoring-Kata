# The Supermarket Receipt Refactoring Kata - PHP version

See the [top level readme](../README.md) for general information about this exercise. This is the PHP version of The
Supermarket Receipt Refactoring Kata

## Installation

The project uses:

- [PHP 8.0+](https://www.php.net/downloads.php)
- [Composer](https://getcomposer.org)

Recommended:

- [Git](https://git-scm.com/downloads)

See [GitHub cloning a repository](https://help.github.com/en/articles/cloning-a-repository) for details on how to
create a local copy of this project on your computer.

```sh
git clone git@github.com:emilybache/SupermarketReceipt-Refactoring-Kata.git
```

or

```shell script
git clone https://github.com/emilybache/SupermarketReceipt-Refactoring-Kata.git
```

Install all the dependencies using composer:

```sh
cd ./SupermarketReceipt-Refactoring-Kata/php
composer install
```

## Dependencies

The project uses composer to install:

- [PHPUnit](https://phpunit.de/)
- [ApprovalTests.PHP](https://github.com/approvals/ApprovalTests.php)
- [PHPStan](https://github.com/phpstan/phpstan)
- [Easy Coding Standard (ECS)](https://github.com/symplify/easy-coding-standard)

## Folders

- `src` - Contains the **ReceiptPrinter** Class which needs to be tested and refactored.
    - `models` - Contains the 11 Classes and Interface which need to be tested and refactored.
- `tests` - Contains **SupermarketTest** which tests **ReceiptPrinter**. **FakeCatalog** is a basic implementation of
  the **SupermarketCatalog** interface allowing the tests to run.

## Testing

PHPUnit is configured for testing, a composer script has been provided. To run the unit tests, from the root of the
project run:

```shell script
composer tests
```

On Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias pu="composer test"`), the same
PHPUnit `composer test` can be run:

```shell script
pu.bat
```

### Tests with Coverage Report

To run all test and generate a html coverage report run:

```shell script
composer test-coverage
```

The coverage report is created in /builds, it is best viewed by opening **index.html** in your browser.

The [XDebug](https://xdebug.org/download) extension is required for coverage report generating.

## Code Standard

Easy Coding Standard (ECS) is used to check for style and code standards,
**[PSR-12](https://www.php-fig.org/psr/psr-12/)** is used. Tip: Only periodically run ECS, when tests are green, to keep
the focus on writing tests, refactoring the code and adding new features.

### Check Code

To check code, but not fix errors:

```shell script
composer check-cs
``` 

On Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias cc="composer check-cs"`), the same
ECS `composer check-cs` can be run:

```shell script
cc.bat
```

### Fix Code

Many code fixes are automatically provided by ECS, if advised to run --fix, the following script can be run:

```shell script
composer fix-cs
```

On Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias fc="composer fix-cs"`), the same
ECS `composer fix-cs` can be run:

```shell script
fc.bat
```

## Static Analysis

PHPStan is used to run static analysis checks. As the code is constantly being refactored only run static analysis
checks once the chapter is complete. Tip: Only periodically run PHPStan, when tests are green, to keep the focus on
writing tests, refactoring the code and adding new features.

```shell script
composer phpstan
```

On Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias ps="composer phpstan"`), the same
PHPStan `composer phpstan` can be run:

```shell script
ps.bat
```

## Approval Tests

ApprovalTests.php can be used to compare the output of the **ReceiptPrinter**, see
[ApprovalTests.PHP](https://github.com/approvals/ApprovalTests.php) for more information, or see the `with_tests`
branch for working examples.

## Start with the refactoring

If you would like to just do the refactoring part of this exercise, you can instead check out the `with_tests` branch.
Those tests have reasonably good coverage and should support most kinds of refactorings you'd like to do.

**Happy coding**!
