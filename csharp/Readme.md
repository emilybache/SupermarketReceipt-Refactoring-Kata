```bash
# Restores reportgenerator and Stryker
dotnet tool restore
```

## Coverage report
https://learn.microsoft.com/en-us/dotnet/core/testing/unit-testing-code-coverage?tabs=linux
https://learn.microsoft.com/en-us/dotnet/core/testing/unit-testing-code-coverage?tabs=linux#integrate-with-net-test
https://learn.microsoft.com/en-us/dotnet/core/testing/unit-testing-code-coverage?tabs=linux#generate-reports


```bash
rm -r TestResults
dotnet test --collect:"XPlat Code Coverage"
reportgenerator -reports:"**/TestResults/*/*.xml" -targetdir:Coverage
open Coverage/index.html
```


## Stryker

```bash
dotnet stryker
```
https://stryker-mutator.io/docs/stryker-net/introduction/
