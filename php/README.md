# The Supermarket Receipt Refactoring Kata - PHP version (With Tests)

See the [top level readme](../README.md) for general information about this exercise.

## Installation

The project uses:

- [PHP 7.2+](https://www.php.net/downloads.php)
  - **ext-spltype** is required for **SplObjectStorage**
- [Composer](https://getcomposer.org)

Recommended:

- [Git](https://git-scm.com/downloads)

Clone the repository

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
- [PHP CodeSniffer](https://github.com/squizlabs/PHP_CodeSniffer/wiki)

## Folders

- `src` - Contains the **ReceiptPrinter** Class which needs to be tested and refactored.
    - `models` - Contains the 11 Classes and Interface which need to be tested and refactored.
- `tests` - Contains **ReceiptPrinterTest** which tests **ReceiptPrinter**.
    - `approvals` - Contain the approved text files for each test method of ReceiptPrinterTest (these should not be
     changed) 
    - `models` - Contains **SupermarketTest** which tests the models, it also contains **FakeCatalog** Class is a basic
     implementation of the **SupermarketCatalog** interface, which allows the tests to run.
        - `approvals` - Contain the approved text files for each test method of SupermarketTest (these should not be
         changed) 

## Testing

PHPUnit is used to run tests, to help this can be run using a composer script. To run the unit tests, from the root of
 the project run:

```shell script
composer test
```

On Windows a batch file has been created, similar to an alias on Linux/Mac (e.g. `alias pu="composer test"`), the same
 PHPUnit `composer test` can be run:

```shell script
pu
```

### Tests with Coverage Report

To run all test and generate a html coverage report run:

```shell script
composer test-coverage
```

The coverage report is created in /builds, it is best viewed by opening **index.html** in your browser.

The [XDEbug](https://xdebug.org/download) extension is required for coverage report generating. 

## Code Standard

Easy Coding Standard (ECS) is used to check for style and code standards,
 **[PSR-12](https://www.php-fig.org/psr/psr-12/)** is used. Tip: Only periodically run ECS, when tests are green, to
 keep the focus on writing tests, refactoring the code and adding new features.

### Check Code

To check code, but not fix errors:

```shell script
composer check-cs
``` 

On Windows a batch file has been created, similar to an alias on Linux/Mac (e.g. `alias cc="composer check-cs"`), the
 same ECS `composer check-cs` can be run:

```shell script
cc
```

### Fix Code

Many code fixes are automatically provided by ECS, if advised to run --fix, the following script can be run:

```shell script
composer fix-cs
```

On Windows a batch file has been created, similar to an alias on Linux/Mac (e.g. `alias fc="composer fix-cs"`), the same
 ECS `composer fix-cs` can be run:

```shell script
fc
```

## Static Analysis

PHPStan is used to run static analysis checks. As the code is constantly being refactored only run static analysis
  checks once the chapter is complete. Tip: Only periodically run PHPStan, when tests are green, to keep the focus on
   writing tests, refactoring the code and adding new features.

```shell script
composer phpstan
```

On Windows a batch file has been created, similar to an alias on Linux/Mac (e.g. `alias ps="composer phpstan"`), the
 same PHPStan `composer phpstan` can be run:

```shell script
ps
```

## Approval Tests

ApprovalTests.php can be used to compare the output of the **ReceiptPrinter**, see
 [ApprovalTests.PHP](https://github.com/approvals/ApprovalTests.php) for more information, or see the `with_tests`
  branch for working examples.

## Start with the refactoring

This is the `with_tests` branch. This branch allows you to do just the refactoring part of this exercise.
 The tests have reasonably good coverage and should support most kinds of refactorings you'd like to do.

**Happy coding**!
