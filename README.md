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

For tests, run `./stack-test-with-db`. The script uses docker to create a temporary Postgres DB to run the test suite including DB tests. If you only want to run tests not involving the DB, you can do so via `stack test`.

The tests are located in the `test/` folder and mirror the structure of the code inside the `src` folder, i.e.
for `src/JSON/Utils.hs` we will have a corresponding test file `test/JSON/Utils/Tests.hs`. All tests are collected and launched via `test/Spec.hs`.

*If running Stack locally on a mac, I would recommend also running `./stack-test-docker-with-db`, as there might be subtle differences that might show on Linux.*

### Azure pipelines

The test suite is compiled and run multi-threaded. However, QuickJS does not like being run in a multi-threaded setting. Due to the way Haskell threads are mapped to OS threads,
a Haskell thread can be run on different OS threads throughout its lifetime. However, this is problematic for QuickJS, which seems to be tied to the OS thread it was started on.

A fix for running locally involves running tests inside `quickjsTest` rather than the `quickjs` environment (defined in `src/Quickjs.hsc`). The difference between those two involves running all the QuickJS IO actions wrapped in a `runInBoundThread`, which binds the Haskell thread to the OS thread it was created in. This works on my local machine, but fails on Azure pipleines with:

```
lost signal due to full pipe: 11
forkOS_entry: interrupted
```

The only fix I've managed to get to working is to link the test executable as non-threaded and make all the tests run sequentially... 
This should not be an issue unless we have a LOT of tests. This involves passing `--flag 'cv-convert:azure'` to stack, when compiling and running the tests on Azure.

## TODO

Set up automatic binary builds for tagged releases?