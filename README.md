# cv-convert

Document converter used inside [CafeVariomeCI4](https://github.com/CafeVariomeUoL/CafeVariomeCI4). [![Build Status](https://dev.azure.com/sb911/cv-convert-haskell/_apis/build/status/CafeVariomeUoL.cv-convert?branchName=master)](https://dev.azure.com/sb911/cv-convert-haskell/_build/latest?definitionId=1&branchName=master)


## Documentation
Haddocs documentation is available at: https://cafevariomeuol.github.io/cv-convert/

To rebuild the docs, commit all changes to your current branch and run `./stack-gh-pages` 

## Tests

For tests, run `stack test`. The tests are located in the `test/` folder and mirror the structure of the code inside the `src` folder, i.e.
for `src/JSON/Utils.hs` we will have a corresponding test file `test/JSON/Utils/Tests.hs`. All tests are collected and run via `test/Spec.hs`.
