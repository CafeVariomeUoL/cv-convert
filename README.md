# cv-convert
[![Build Status](https://dev.azure.com/sb911/cv-convert-haskell/_apis/build/status/CafeVariomeUoL.cv-convert?branchName=master)](https://dev.azure.com/sb911/cv-convert-haskell/_build/latest?definitionId=1&branchName=master)

Document converter used inside [CafeVariomeCI4](https://github.com/CafeVariomeUoL/CafeVariomeCI4). 

## Documentation
Haddocs documentation is available at: https://cafevariomeuol.github.io/cv-convert/

To rebuild the docs, commit all changes to your current branch and run `./stack-gh-pages` 


## Compiling

If you have Haskell Stack set up locally, you can simply run `stack build`. However, to ensure (hopefully) consistent build and not having to deal with external dependencies,
we can use docker to build the project. This is also the way to build linux binaries on mac. First, we need to build the docker image that will be used by stack:

```docker build --tag static-haskell-alpine-8.8.3 .```

Afterwards, we can run

```stack build --docker --docker-image static-haskell-alpine-8.8.3```

## Tests

For tests, run `./stack-test-with-db`. The tests are located in the `test/` folder and mirror the structure of the code inside the `src` folder, i.e.
for `src/JSON/Utils.hs` we will have a corresponding test file `test/JSON/Utils/Tests.hs`. All tests are collected and run via `test/Spec.hs`.

*If running Stack locally on a mac, I would recommend also running `stack test --docker --docker-image static-haskell-alpine-8.8.3`, as there might be subtle differences that might show up when testing on Linux. Since we use, Azure pipelines CI, which currently build on a Linux VM, the CI tests might fail even if the tests ran succesfully on your local mac.*

## TODO

Set up DB tests for stack test --docker?
Set up automatic binary builds for tagged releases?